package com.ditcalendar.bot.endpoint

import com.ditcalendar.bot.config.config
import com.ditcalendar.bot.config.dit_calendar_server_url
import com.ditcalendar.bot.config.dit_calendar_user_name
import com.ditcalendar.bot.config.dit_calendar_user_password
import com.github.kittinunf.fuel.core.ResponseDeserializable
import com.github.kittinunf.fuel.httpPost
import com.github.kittinunf.result.Result
import com.google.gson.Gson


class AuthEndpoint {

    private val config by config()

    fun getToken() : String? {

        val ditCalendarUrl = config[dit_calendar_server_url]
        val userName = config[dit_calendar_user_name]
        val password = config[dit_calendar_user_password]

        val (_, _, result) = "$ditCalendarUrl/authenticate/authentication-methods/password/token"
                .httpPost()
                .body("{\"user\":\"$userName\",\"password\":\"$password\"}")
                .responseObject(JWT.Deserializer())

        return when (result) {
            is Result.Failure -> {
                val ex = result.getException()
                println(ex)
                null
            }
            is Result.Success -> {
                val jwt = result.get()
                if(jwt.jrStatus == "Ok")
                    jwt.jrData.token
                else
                    null
            }
        }
    }

    data class JWT(val jrStatus : String, val jrData : Token) {
        class Deserializer : ResponseDeserializable<JWT> {
            override fun deserialize(content: String): JWT? = Gson().fromJson(content, JWT::class.java)
        }
    }

    data class Token(val token : String)
}