package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.data.DitCalendar
import com.github.kittinunf.fuel.httpGet
import com.github.kittinunf.result.Result

class CalendarEndpoint {

    private val config by config()


    fun readCalendar(calendarId: Long) : DitCalendar? {

        val ditCalendarUrl = config[dit_calendar_server_url]

        val (_, _, result) = "$ditCalendarUrl/calendarentries/1"
                .httpGet()
                .responseObject(DitCalendar.Deserializer())

        return when (result) {
            is Result.Failure -> {
                val ex = result.getException()
                println(ex)
                null
            }
            is Result.Success -> result.get()
        }
    }
}