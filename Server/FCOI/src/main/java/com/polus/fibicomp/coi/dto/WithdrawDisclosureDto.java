package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;
import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class WithdrawDisclosureDto {

	private Timestamp certifiedAt;

	private Date expirationDate;

	private Timestamp updateTimestamp;

	private String reviewStatusCode;

	private String reviewStatusDescription;

}