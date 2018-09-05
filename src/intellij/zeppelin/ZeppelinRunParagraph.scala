package intellij.zeppelin

import com.intellij.openapi.actionSystem.AnActionEvent
import com.intellij.openapi.editor.Editor

class ZeppelinRunParagraph extends ZeppelinAction {

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = currentEditor(anActionEvent)
    val api = zeppelin(anActionEvent)
    findNotebook(editor) map { notebook =>
      val paragraph_start_line = findPreviousLineMatching(editor, text => text.matches(Paragraph.ParagraphFlag.regex)).getOrElse(-1)
      val paragraph_start_flag = editor.getDocument.getCharsSequence.subSequence(
        editor.getDocument.getLineStartOffset(paragraph_start_line),
        editor.getDocument.getLineEndOffset(paragraph_start_line)
      ).toString
      val paragraph_end_line = findNextLineMatching(editor, paragraph_start_line + 1, text => text == Paragraph.paragraphFooter).getOrElse(-1)

      val codeFragment = currentCodeFragment(editor, paragraph_start_line, paragraph_end_line - 1)
      api.getParagraphByFlag(notebook, paragraph_start_flag) map { paragraph =>
        api.deleteParagraph(notebook, paragraph) map { _ =>
          api.createParagraph(notebook, codeFragment.content, Some(paragraph.index)) map { newParagraph =>
            runWriteAction(anActionEvent) { _ =>
              replaceParagraph(editor, paragraph_start_line, paragraph_end_line, newParagraph)
            }

            api.runParagraph(notebook, newParagraph) map { result =>
              runWriteAction(anActionEvent) { _ =>
                replaceOutput(editor, paragraph_end_line + 2, result.markerText)
              }
            }
          }
        }
      }
    }
  }
}


