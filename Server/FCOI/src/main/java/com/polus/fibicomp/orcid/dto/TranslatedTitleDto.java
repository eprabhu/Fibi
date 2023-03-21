package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "value",
    "language-code"
})
public class TranslatedTitleDto {

	@JsonProperty("value")
    private String value;

	@JsonProperty("language-code")
    private String languageCode;

    @JsonProperty("value")
    public String getValue() {
        return value;
    }

    @JsonProperty("value")
    public void setValue(String value) {
        this.value = value;
    }

    @JsonProperty("language-code")
	public String getLanguageCode() {
		return languageCode;
	}

    @JsonProperty("language-code")
	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}

}
