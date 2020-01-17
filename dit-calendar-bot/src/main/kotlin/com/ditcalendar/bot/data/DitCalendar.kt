package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.google.gson.Gson
import java.util.*

data class DitCalendar(val description : String,
                       //val tasks : List<Long>,
                       val startDate : Date) : Base() {
    class Deserializer: ResponseDeserializable<DitCalendar> {
        override fun deserialize(content: String): DitCalendar? = Gson().fromJson(content, DitCalendar::class.java)
    }

    override fun toStringInMarkdown(): String =
            """
                *$description*
                *Datum*:
            """.trimIndent()
}