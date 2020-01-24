package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.config.dit_calendar_user_name
import com.ditcalendar.bot.config.dit_calendar_user_password
import com.github.kittinunf.fuel.httpPost
import com.github.kittinunf.result.Result
import com.github.kittinunf.result.flatMap
import kotlinx.serialization.Serializable
import kotlinx.serialization.UnstableDefault
import kotlinx.serialization.json.Json


class AuthEndpoint {

    private val config by config()

    private val ditCalendarUrl = config[dit_calendar_server_url]
    private val userName = config[dit_calendar_user_name]
    private val password = config[dit_calendar_user_password]

    @UnstableDefault
    fun getToken(): Result<String, Exception> {

        val (_, _, result) = "$ditCalendarUrl/authenticate/authentication-methods/password/token"
                .httpPost()
                .body("{\"user\":\"$userName\",\"password\":\"$password\"}")
                .responseString()


        return result.flatMap {
            Result.of<String, Exception> {
                val jwt = Json.parse(JWT.serializer(), result.get())
                if (jwt.jrStatus == "Ok")
                    jwt.jrData.token
                else
                    throw Exception("jrStatus: ${jwt.jrStatus}")
            }
        }
    }

    @Serializable
    private data class JWT(val jrStatus: String, val jrData: Token) {
        @Serializable
        data class Token(val token: String)
    }
}