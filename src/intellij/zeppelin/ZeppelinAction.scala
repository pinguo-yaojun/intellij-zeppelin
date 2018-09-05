package intellij.zeppelin

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.command.{CommandProcessor, UndoConfirmationPolicy}
import com.intellij.openapi.editor.{Document, Editor}
import com.intellij.openapi.util.Computable

import scala.collection.immutable

abstract class ZeppelinAction extends AnAction with IdeaDocumentApi {

  def zeppelin(anActionEvent: AnActionEvent): ZeppelinApi = {
    ZeppelinConnection.connectionFor(anActionEvent.getProject).api
  }

  def findNotebook(editor: Editor): Option[Notebook] = precedingLines(editor).flatMap(x => Notebook.parse(x._2)).headOption

  def findParagraph(editor: Editor): Option[Paragraph] = precedingLines(editor).flatMap(x => Paragraph.parse(x._2)).headOption

  private def precedingLines(editor: Editor): immutable.Seq[(Int, String)] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    Range(currentLine, 1, -1).map { line =>
      val start = editor.getDocument.getLineStartOffset(line - 1)
      val end = editor.getDocument.getLineStartOffset(line)
      (line, editor.getDocument.getCharsSequence.subSequence(start, end).toString)
    }.map(x => x.copy(_2 = x._2.stripLineEnd))
  }

  def findNote(editor: Editor, line: Int): Option[Notebook] = {
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    val linesInReverse = Range(currentLine, 1, -1).map { line =>
      val start = editor.getDocument.getLineStartOffset(line - 1)
      val end = editor.getDocument.getLineStartOffset(line)
      editor.getDocument.getCharsSequence.subSequence(start, end).toString
    }.map(_.stripLineEnd)

    linesInReverse.flatMap(Notebook.parse).headOption
  }

  protected def updateNotebookMarker(editor: Editor, notebook: Notebook): Unit = {
    findPreviousLineMatching(editor, text => Notebook.parse(text).isDefined).foreach { line =>
      replaceLine(editor, line, notebook.markerText)
    }
  }

  def insertBefore(editor: Editor, notebook: Notebook, url:String): Unit = {
    val offset = editor.getCaretModel.getOffset
    val currentLine = editor.getCaretModel.getLogicalPosition.line
    val lineStartOffset = editor.getDocument.getLineStartOffset(currentLine)

    val message = notebook.notebookHeader(url)
    editor.getDocument.insertString(lineStartOffset, message)
    editor.getCaretModel.moveToOffset(offset + message.length)
  }

  def replaceParagraph(editor: Editor, startLine: Int, endLine: Int, paragraph: Paragraph): Unit = {
    val message = paragraph.markerText
    editor.getDocument.replaceString(
      editor.getDocument.getLineStartOffset(startLine),
      editor.getDocument.getLineEndOffset(endLine),
      message)
  }

  def replaceOutput(editor: Editor, line: Int, withText: String): Unit = {
    val existOutput = editor.getDocument.getCharsSequence.subSequence(
      editor.getDocument.getLineStartOffset(line + 1),
      editor.getDocument.getLineEndOffset(line + 1)
    ).toString.equals("Output:")

    if(existOutput) {
      val start_line = line - 1
      val end_line = findNextLineMatching(editor, 0, text => text.replaceAll("\n", "") == "*/").getOrElse(-1)
      editor.getDocument.replaceString(
        editor.getDocument.getLineStartOffset(start_line),
        editor.getDocument.getLineEndOffset(end_line),
        withText
      )

    }
    else replaceLine(editor, line - 1, withText)
  }

  protected def runWriteAction(anActionEvent: AnActionEvent)(f: Document => Unit): Unit = ApplicationManager.getApplication.runWriteAction{
    val document = currentDocument(currentFileIn(anActionEvent.getProject))
    new Computable[Unit] {
      override def compute(): Unit = {
        CommandProcessor.getInstance().executeCommand(
          anActionEvent.getProject,
          new Runnable {
            override def run(): Unit = {
              f(document)
            }
          },
          "Modified from Zeppelin Idea plugin",
          "ZeppelinIdea",
          UndoConfirmationPolicy.DEFAULT,
          document
        )
      }
    }
  }
}
