package intellij.zeppelin
import com.intellij.openapi.actionSystem.AnActionEvent

class ZeppelinGetNoteAction extends ZeppelinAction {

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = currentEditor(anActionEvent)
    val api = zeppelin(anActionEvent)
    findNotebook(editor).map { note =>
      api.getNotebook(note.name) map { notebook =>
        runWriteAction(anActionEvent) { _ =>
          val currentLine = findPreviousLineMatching(editor, text => text.matches(""".*//Notebook:([\w/-]+).*""")).getOrElse(-1)
          val message = notebook.notebookHeader(api.url)
          replaceLine(editor, currentLine, message + "\n\n")
//          val lineStartOffset = editor.getDocument.getLineStartOffset(currentLine + 1)
//          editor.getCaretModel.moveToOffset(lineStartOffset + message.length)

          api.listParagraphs(notebook) map { paragraphs =>
            runWriteAction(anActionEvent){ _ =>
              updateNotebookMarker(editor, notebook.copy(size = notebook.size + paragraphs.size))
              insertAfterFragment(editor, CodeFragment(SingleLine, ""), paragraphs.filter(_.text.nonEmpty).map(p => p.markerText).mkString("\n"))
            }
          }
        }
      }
    }
  }
}
