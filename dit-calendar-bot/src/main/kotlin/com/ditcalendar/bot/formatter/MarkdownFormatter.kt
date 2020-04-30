package com.ditcalendar.bot.formatter

import com.ditcalendar.bot.data.*
import org.eclipse.microprofile.config.inject.ConfigProperty
import javax.enterprise.context.ApplicationScoped


@ApplicationScoped
class MarkdownFormatter {

    @ConfigProperty(name = "bot.name")
    private lateinit var botName: String

    fun Task.toMarkdown(): String =
            when (this) {
                is TaskForAssignment ->
                    """
                    _Task_: $description [assign me](https://t.me/$botName?start=assign_$taskId)
                    _Datum_: $startTime
                """.trimIndent()
                is TaskForUnassignment ->
                    """
                    _Task_: $description
                    _Datum_: $startTime
                """.trimIndent()
                is TaskAfterUnassignment ->
                    """
                    *erfolgreich ausgetragen von*:
                    _Task_: $description
                    _Datum_: $startTime
                """.trimIndent()
            }


    fun Tasks.toMarkdown(): String = joinToString(separator = System.lineSeparator()) { it.toMarkdown() }

    fun DitCalendar.toMarkdown(): String =
            """
            *$description*
            *Datum*: $startDate
            
        """.trimIndent() + tasks.toMarkdown()
}