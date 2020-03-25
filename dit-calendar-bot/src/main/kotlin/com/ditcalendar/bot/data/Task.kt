package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.data.core.DateSerializer
import kotlinx.serialization.Serializable
import java.util.*

typealias Tasks = List<Task>

@Serializable
data class Task(val taskId: Long,
                val description: String,
                @Serializable(with = DateSerializer::class)
                val startTime: Date,
                @Serializable(with = DateSerializer::class)
                val endTime: Date? = null) : Base() {

    override fun toStringInMarkdown(): String =
            """_Task_: $description
               _Datum_: $startTime
            """.trimIndent()

    fun toStringForCalendarInMarkdown(): String =
            """
                _Task_: $description [assignMe](https://t.me/ditcalendarbot?start=assign_$taskId)
                _Datum_: $startTime
            """.trimIndent()

}

fun Tasks.toStringInMarkdown(): String {
    var response = ""
    this.forEach { response += it.toStringForCalendarInMarkdown() + System.lineSeparator() }
    return response
}