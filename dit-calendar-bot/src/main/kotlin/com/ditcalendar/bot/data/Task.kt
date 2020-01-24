package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.data.core.DateSerializer
import kotlinx.serialization.Serializable
import java.util.*

typealias Tasks = List<Task>

@Serializable
data class Task(val description: String,
                @Serializable(with = DateSerializer::class)
                val startTime: Date,
                @Serializable(with = DateSerializer::class)
                val endTime: Date? = null) : Base() {

    override fun toStringInMarkdown(): String =
            """
                _Task_: $description
                _Datum_: $startTime
            """.trimIndent()
}

fun Tasks.toStringInMarkdown(): String {
    var response = ""
    this.forEach { response += it.toStringInMarkdown() + System.lineSeparator() }
    return response
}