package com.polus.kcintegration.exception.vo;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ExceptionResponse {

	private String errorMessage;
	private String errorCode;
	private String timestamp;

}
