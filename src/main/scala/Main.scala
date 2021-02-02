import java.io.{File, FileFilter, FilenameFilter}

import scala.xml.XML


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

  //Load csproj files
  def foo = enumerateAllFiles(new File("D:\\sd\\ConvertDllReference"), IsCSProj)
    //Parse xml files
    .map(file => (file.getAbsolutePath, file.getName,XML.loadFile(file)))
    //Locate dll references
    //Gets file path (For linking), project guid, And reference tags (To remove them?)
    .map({
      case (path, name, xml) => (path, name, (xml \ "PropertyGroup" \ "ProjectGuid").text, (xml \ "ItemGroup" \ "Reference" ))
    })

  //Correlate to csproj (in list)
  //Delete dll reference
  //Insert csproj reference
}