package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "error-code",
    "error-message",
    "transient"
})
public class ExternalIdNormalizedErrorDto {

	@JsonProperty("error-code")
    private String errorCode;

	@JsonProperty("error-message")
    private String errorMessage;

    @JsonProperty("transient")
    private boolean transientFlag;

    @JsonProperty("error-code")
	public String getErrorCode() {
		return errorCode;
	}

    @JsonProperty("error-code")
	public void setErrorCode(String errorCode) {
		this.errorCode = errorCode;
	}

    @JsonProperty("error-message")
	public String getErrorMessage() {
		return errorMessage;
	}

    @JsonProperty("error-message")
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

    @JsonProperty("transient")
	public boolean isTransientFlag() {
		return transientFlag;
	}

    @JsonProperty("transient")
	public void setTransientFlag(boolean transientFlag) {
		this.transientFlag = transientFlag;
	}

}
