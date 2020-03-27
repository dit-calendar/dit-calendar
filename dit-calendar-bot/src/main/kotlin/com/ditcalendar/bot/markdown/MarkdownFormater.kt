package com.ditcalendar.bot.markdown

import com.ditcalendar.bot.config.bot_name
import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.data.DitCalendar
import com.ditcalendar.bot.data.Task
import com.ditcalendar.bot.data.Tasks


private val config by config()

private val botName = config[bot_name]

fun Task.toStringInMarkdown(): String =
        """_Task_: $description
           _Datum_: $startTime
           [unassign me](https://t.me/$botName?start=unassign_$taskId)
        """.trimIndent()

fun Task.toStringForCalendarInMarkdown(): String =
        """
            _Task_: $description [assign me](https://t.me/$botName?start=assign_$taskId)
            _Datum_: $startTime
        """.trimIndent()


fun Tasks.toStringInMarkdown(): String {
    var response = ""
    this.forEach { response += it.toStringForCalendarInMarkdown() + System.lineSeparator() }
    return response
}

fun DitCalendar.toStringInMarkdown(): String =
        """
            *$description*
            *Datum*: $startDate
            
        """.trimIndent() + tasks.toStringInMarkdown()