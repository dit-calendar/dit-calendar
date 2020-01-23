package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.ditcalendar.bot.data.core.DateSerializer
import java.util.*

import kotlinx.serialization.*

@Serializable
data class DitCalendar(val description : String,
                       @Serializable(with = DateSerializer::class)
                       val startDate : Date) : Base() {

    override fun toStringInMarkdown(): String =
            """
                *$description*
                *Datum*: $startDate
            """.trimIndent()
}