package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.google.gson.Gson
import java.util.*

data class Task(val description : String,
                val startTime : Date,
                val endTime : Date) : Base() {
    class Deserializer: ResponseDeserializable<Array<Task>> {
        override fun deserialize(content: String): Array<Task>? = Gson().fromJson(content, Array<Task>::class.java)
    }

    override fun toStringInMarkdown(): String =
            """
                    _Task_: $description
                    _Datum_: $startTime
            """.trimIndent()
}