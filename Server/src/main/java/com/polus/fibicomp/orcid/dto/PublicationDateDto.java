package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "year",
    "month",
    "day"
})
public class PublicationDateDto {

	@JsonProperty("year")
    private YearDto year;

    @JsonProperty("month")
    private MonthDto month;

    @JsonProperty("day")
    private DayDto day;

    @JsonProperty("year")
	public YearDto getYear() {
		return year;
	}

    @JsonProperty("year")
	public void setYear(YearDto year) {
		this.year = year;
	}

    @JsonProperty("month")
	public MonthDto getMonth() {
		return month;
	}

    @JsonProperty("month")
	public void setMonth(MonthDto month) {
		this.month = month;
	}

    @JsonProperty("day")
	public DayDto getDay() {
		return day;
	}

    @JsonProperty("day")
	public void setDay(DayDto day) {
		this.day = day;
	}

}
