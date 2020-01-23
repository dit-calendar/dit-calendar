package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.data.core.DateSerializer
import kotlinx.serialization.Serializable
import java.util.*

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

fun List<Task>.toStringInMarkdown(): String =
        this.joinToString { it.toStringInMarkdown() + System.lineSeparator() }