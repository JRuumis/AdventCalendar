package jruumis.adventofcode.year2022

import scala.annotation.tailrec

object Day7NoSpaceOnDevice extends App {

    val fileSystemBrowseContent = scala.io.Source.fromFile("./Sources/Day7NoSpaceOnDevice.txt").getLines().toList

    val folderNamePattern = "\\$ cd ([0-9a-zA-Z_/.]+)".r
    val fileNamePattern = "([0-9]+) ([0-9a-zA-Z_/.]+)".r

    @tailrec
    def addCurrentFolder(
                            curFolderPath: List[String],
                            browseContent: List[String],
                            accu: List[(String, List[String])]
                        ): List[(String, List[String])] = browseContent match {
        case Nil => accu
        case "$ cd /" :: rest => addCurrentFolder(List("/"), rest, ("$ cd /", List("/")) :: accu)
        case "$ cd .." :: rest => addCurrentFolder(curFolderPath.tail, rest, ("$ cd ..", curFolderPath.tail) :: accu)
        case b :: rest if folderNamePattern matches b => addCurrentFolder(folderNamePattern.findFirstMatchIn(b).get.group(1) :: curFolderPath, rest, (b, folderNamePattern.findFirstMatchIn(b).get.group(1) :: curFolderPath) :: accu)
        case b :: rest => addCurrentFolder(curFolderPath, rest, (b, curFolderPath) :: accu)
    }

    val browseCommandsWithCurrentFolder = addCurrentFolder(List("/"), fileSystemBrowseContent, List())

    val browseCommandsWithFolderAndFileSize = browseCommandsWithCurrentFolder map { case (command, folderPath) => command match {
        case fileName if fileNamePattern matches fileName => (command, folderPath, Some(fileNamePattern.findFirstMatchIn(fileName).get.group(1).toInt))
        case _ => (command, folderPath, None)
    }
    }

    @tailrec
    def expandList[A](
                         list: List[A],
                         accu: List[List[A]] = List()
                     ): List[List[A]] = list match {
        case Nil => accu
        case l :: rest => expandList(rest, list :: accu)
    }

    val foldersAndFileSizes = browseCommandsWithFolderAndFileSize
        .filter { case (_, _, sizeOption) => sizeOption.isDefined }
        .map { case (_, folderPath, sizeOption) => (folderPath, sizeOption.get) }
        .flatMap { case (folderPath, size) => expandList(folderPath).map((_, size)) } // to add file size to parent folders as well

    val folderSizes = foldersAndFileSizes
        .groupBy { case (path, _) => path }
        .view.mapValues(a => a.map { case (_, size) => size }.sum)
        .toMap

    val folderSumsAllLessThan100000 = folderSizes
        .filter { case (_, size) => size <= 100000 }
        .map { case (_, size) => size }
        .sum

    println(s"Summary folder sizes with size not exceeding 100000 is ${folderSumsAllLessThan100000}")

    val totalDiscSpace = 70000000
    val spaceNeeded = 30000000
    val spaceTaken = folderSizes(List("/"))
    val spaceToBeFreed = if (spaceNeeded - (totalDiscSpace - spaceTaken) > 0) spaceNeeded - (totalDiscSpace - spaceTaken) else 0

    val folderSizesBiggerThanSpaceToBeFreed = folderSizes.map { case (_, size) => size }.filter(_ >= spaceToBeFreed)
    val smallestFolderToDelete = folderSizesBiggerThanSpaceToBeFreed.min

    println(s"The size of the smallest folder at least ${spaceToBeFreed} big is ${smallestFolderToDelete}")
}
