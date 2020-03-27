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
                val endTime: Date? = null) : Base()