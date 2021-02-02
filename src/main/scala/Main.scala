import java.io.{File, FileFilter, FilenameFilter}
import java.nio.file.Path
import scala.xml.{Elem, Node, NodeSeq, UnprefixedAttribute, XML}
import scala.xml.transform.{RewriteRule, RuleTransformer}

object IsFile extends FileFilter {
  override def accept(pathname: File): Boolean = pathname.isFile
}
object IsDir extends FileFilter {
  override def accept(pathname: File): Boolean = pathname.isDirectory
}
object IsCSProj extends FilenameFilter {
  override def accept(dir: File, name: String): Boolean = name.endsWith(".csproj")
}

object Main extends App {

  def debug[A](input : A) : A = {
    println(input)
    input
  }

  def enumerateAllFiles(dir : File, filter : FilenameFilter): List[File] = {
    val files = dir.listFiles(IsDir).flatMap(d => enumerateAllFiles(d, filter))

    dir.listFiles(filter).concat(files).toList
  }

  case class CSProj(file : File) {
    val xml: Elem = XML.loadFile(file)
    val name: String = file.getName.substring(0, file.getName.lastIndexOf('.'))
    val path : Path = file.toPath
    val guid: String =(xml \ "PropertyGroup" \ "ProjectGuid").text
    val dependencyNames: Seq[String] = (xml \ "ItemGroup" \ "Reference" ).map(ref => (ref \@ "Include").takeWhile(c => c != ','))
    def relativeTo(other : String): String = Path.of(other).relativize(path).toString
  }

  //Load csproj files
  def getProjInfo= enumerateAllFiles(new File("ConvertDllReference"), IsCSProj)
    //Parse xml files, get path, and proj name (Minus extension)
    .map(file => new CSProj(file))

  //Resolve the project guid/relative path for each dependency listed
  def resolveProjRefs = getProjInfo.map { proj =>
    //Gets the csProj object for each named dependency, if it's ours (exclude system/nuget packages)
    val ourDependencies = getProjInfo.filter(p => proj.dependencyNames.contains(p.name))
    val ourDependencyNames = ourDependencies.map(p => p.name)

    val projRefXml =
      <ItemGroup>
        { ourDependencies.map { dep =>
            <ProjectReference Include={dep.relativeTo(proj.path.toString)}>
              <Project>{dep.guid}</Project>
              <Name>{dep.name}</Name>
            </ProjectReference>
          }
        }
      </ItemGroup>

    val newXml = new RuleTransformer(Main.addProjRefRule(projRefXml), Main.removeDllRefRule(ourDependencyNames)).transform(proj.xml)
    newXml.foldRight(0)((x,n) => {
      XML.save(proj.name+n+".xml", x)
      n+1
    })

    println(newXml)
   //Returns the csProj, the resolved csproj dependencies, and the new project reference xml
    (proj, newXml)
  }

  def removeDllRefRule(dependencyNames : List[String]) = new RewriteRule {
    override def transform(n: Node): collection.Seq[Node] = n match {
      case elem : Elem if elem.label == "Reference" && dependencyNames.foldRight(false){ (name, acc) => acc || name.startsWith(elem \@ "Include") } => NodeSeq.Empty
      case n => n
    }
  }

  def addProjRefRule(projRefXML : Node) = new RewriteRule {
    override def transform(n: Node): collection.Seq[Node] = n match {
      case elem : Elem if elem.label == "Project" => elem.copy(child =  projRefXML ++ elem.child)
      case n => n
    }
  }

  //Delete dll reference
  //Insert csproj reference
}