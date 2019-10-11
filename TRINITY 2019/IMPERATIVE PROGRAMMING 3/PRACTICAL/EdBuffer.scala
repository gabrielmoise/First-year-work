

// EdBuffer.scala
// Copyright (c) 2015 J. M. Spivey
// Amended 2017 by P.G. Jeavons

import java.io.{Reader, Writer, FileReader, FileWriter, IOException}

/** The state of an editing session 
 *  (this class is the model part of the MVC architecture) */
class EdBuffer {
    /** The text being edited. */
    private val text = new PlaneText()

    /** The display. */
    private var display: Display = null

    /** Register a display */
    def register(display: Display) { this.display = display }
    
    // State components that are preserved by undo and redo

    // TASK 3
    /** Position of the mark */
    private var _mark = 0

    // TASK 4
    /** The text that has been most recently copied */
    private var _copyText : Text.Immutable = text.getRange(0,0) // initialisation

    /** Current editing position. */
    private var _point = 0

    // State components that are not restored on undo

    /** File name for saving the text. */
    private var _filename = ""

    /** Dirty flag */
    private var modified = false

    /** Mark the buffer as modified */
    private def setModified() { modified = true }

    /** Test whether the text is modified */
    def isModified = modified
    

    // Display update
    
    /** Extent that the display is out of date. */
    private var damage = EdBuffer.CLEAN
    
    /** If damage = REWRITE_LINE, the line that should be rewritten */
    private var damage_line = 0

    /** Note damage to the display. */
    private def noteDamage(rewrite: Boolean) {
        val newdamage = 
            if (rewrite) EdBuffer.REWRITE else EdBuffer.REWRITE_LINE
        damage = Math.max(damage, newdamage)
        damage_line = text.getRow(point)
    }
    
    /** Force a display rewrite at next update */
    def forceRewrite() { noteDamage(true) }

    /** Update display with cursor at current point */
    def update() { update(point) }

    /** Update display with cursor at specified position */
    def update(pos: Int) {
        display.refresh(damage, text.getRow(pos), text.getColumn(pos))
        damage = EdBuffer.CLEAN
    }
    
    /** Initialise display */
    def initDisplay() {
        noteDamage(true)
        update()
    }

    // TASK 4
    // Function that tests whether a position is a valid or not (we consider '\n' invalid)
    def valid(pos : Int) : Boolean = {
	if (pos < 0) return false
	val row = getRow(pos)
	val col = getColumn(pos)
	if ((col < 0) || (col >= getLineLength(row) - 1)) return false
	return true
    }

    // Accessors

    // TASK 3 
    /** Methods to access and modify the position of the mark */
    def mark = _mark
   
    def mark_=(mark: Int) {
	_mark = mark
    }

    // TASK 4
    /** Methods to access and modify the most recent copied/cut text */
    def copyText = _copyText

    def copyText_=(copyText : Text.Immutable) {
	_copyText = copyText
    }

    def point = _point

    def point_=(point: Int) {
        if (damage == EdBuffer.REWRITE_LINE && getRow(point) != damage_line)
            damage = EdBuffer.REWRITE
        _point = point
    }

    def filename = _filename

    private def filename_=(filename: String) { _filename = filename }


    // Delegate methods for text
    
    def charAt(pos: Int) = text.charAt(pos)

    def getRow(pos: Int) = text.getRow(pos)

    def getColumn(pos: Int) = text.getColumn(pos)
    
    def getPos(row: Int, col: Int) = text.getPos(row, col)

    def length = text.length

    def getLineLength(row: Int) = text.getLineLength(row)

    def getRange(pos: Int, len: Int) = text.getRange(pos, len)

    def numLines = text.numLines

    def fetchLine(n: Int, buf: Text) { text.fetchLine(n, buf) }

    def writeFile(out: Writer) { text.writeFile(out) }

    // Mutator methods

    /** TASK 2: Transpose two characters */
    def transpose(pos: Int) = {
	// we need to note the display to rewrite the line
	noteDamage(false)
	text.transpose(pos)
	setModified()
    }

    // TASK 5
    /** Search for a given String in the buffer */
    def searchText(word : String, pos : Int) : Int = {
	var len = word.length
	var found = false 
	if (len == 0) return -1 // we got an empty word to search => invalid search
	if (len > length) return -1 // we got a bigger word than our text => can't find it
	// We first search from the current editing point until the end of the buffer
	var current = Math.max(pos + 1,len-1) // we start the search from where the next occurrence can end 
	while ((current <= length-1) && !(found))
		{
		   var str : String = ""
		   for (i <- current-len+1 to current) str += charAt(i) 
		   if (str == word) found = true
		       else current += 1
		}
	if (found) return current
	// If we didn't find anything so far, we wrap around and start the search up to the editing point
	current = len-1
	while ((current <= pos) && !(found)) 
		{
		   var str : String = ""
		   for (i <- current-len+1 to current) str += charAt(i) 
		   if (str == word) found = true
		       else current += 1
		}
	if (found) return current // notice that current can be equal to pos and we have a separate case for this 
	return -1 // we didn't find the text in the buffer
   }

    /** Delete a character */
    def deleteChar(pos: Int) {
        val ch = text.charAt(pos)
	if (mark > pos) mark -= 1 // TASK 3
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.deleteChar(pos)
        setModified()
    }

    /** Delete a range of characters. */
    def deleteRange(pos: Int, len: Int) {
	if (mark >= pos) mark -= Math.min(len,mark-pos) // TASK 3
        noteDamage(true)
        text.deleteRange(pos, len)
        setModified()
    }
    
    /** Insert at current value of point */
    def insert(a: Any) { 
        a match {case ch: Char => insert(point, ch)
                 case s : String => insert(point, s)
        }
    }    
  
    /** Insert a character at a specified position */
    def insert(pos: Int, ch: Char) {
	if (mark > pos) mark += 1 // TASK 3
        noteDamage(ch == '\n' || getRow(pos) != getRow(point))
        text.insert(pos, ch)
        setModified()
    }
    
    /** Insert a string */
    def insert(pos: Int, s: String) {
	if (mark > pos) mark += s.length // TASK 3
        noteDamage(true)
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert an immutable text. */
    def insert(pos: Int, s: Text.Immutable) {
	if (mark > pos) mark += s.length // TASK 3
        noteDamage(true)
        text.insert(pos, s)
        setModified()
    }
    
    /** Insert a Text. */
    def insert(pos: Int, t: Text) {
	if (mark > pos) mark += t.length // TASK 3
        noteDamage(true)
        text.insert(pos, t)
        setModified()
    }
    
     /** Load a file into the buffer. */
    def loadFile(name: String): Boolean = {
        filename = name
        
        try {
            val in = new FileReader(name)
            text.clear()
            text.insertFile(0, in)
            in.close()
            modified = false
            noteDamage(true)
            return true
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't read file '%s'", name)
                return false
        }       
    }
    
    /** Save buffer contents to a file */
    def saveFile(name: String): Boolean = {
        filename = name
    
        try {
            val out = new FileWriter(name)
            text.writeFile(out)
            out.close()
            modified = false
            return true
        } catch {
            case e: IOException =>
                MiniBuffer.message(display, "Couldn't write file '%s'", name)
                return false
        }
    }

    /** Make a Memento that records the current state */
    def getState() = new Memento()
    
    /** An immutable record of the editor state at some time.  The state that
     * is recorded consists of just the current point. */
    class Memento {
        private val pt = point
	private val mk = mark // TASK 3
        
        /** Restore the state when the memento was created */
        def restore() 
	   { 
	      point = pt 
	      mark = mk // TASK 3
	   }
    }
    
    
}
   
object EdBuffer {
    /** Possible value for damage. */
    val CLEAN = 0
    val REWRITE_LINE = 1
    val REWRITE = 2
}
