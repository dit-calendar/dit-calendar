package com.ditcalendar.bot.data

import com.ditcalendar.bot.data.core.Base
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.google.gson.Gson
import java.time.Instant

data class DitCalendar(val description : String
                       //val tasks : List<Long>,
                       //val startDate : Instant,
                       //val endDate : Instant
) : Base() {
    class Deserializer: ResponseDeserializable<DitCalendar> {
        override fun deserialize(content: String): DitCalendar? = Gson().fromJson(content, DitCalendar::class.java)
    }
}