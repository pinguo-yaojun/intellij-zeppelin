package intellij.zeppelin

import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.editor.Editor

import scala.util.Try

class ZeppelinAddParagraph extends ZeppelinAction {

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {

    val editor = currentEditor(anActionEvent)
    val api = zeppelin(anActionEvent)
    findNotebook(editor) map { notebook =>
      val codeFragment = currentCodeFragment(editor)
      api.createParagraph(notebook, codeFragment.content.replaceAll("\n\\s+", "\n"), Some(notebook.size)) map { paragraph =>
        runWriteAction(anActionEvent) { _ =>
          updateNotebookMarker(editor, notebook.copy(size = notebook.size + 1))
          insertAfterFragment(editor, codeFragment,  "\n" + Paragraph.paragraphFooter + "\n")
        }

        api.runParagraph(notebook, paragraph) map { result =>
          val paragraph_end_line = findNextLineMatching(editor, 0, text => text == Paragraph.paragraphFooter).getOrElse(-1)
          runWriteAction(anActionEvent) { _ =>
            replaceOutput(editor, paragraph_end_line + 1, result.markerText)
          }
        }
      }
    }
  }
}


