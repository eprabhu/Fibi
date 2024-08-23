package com.polus.fibicomp.globalentity.dto;

import java.sql.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class OtherDetailsRequestDTO {

	private Integer entityId;
	private Date startDate;
	private Date incorporationDate;
	private String incorporatedIn;
	private String congressionalDistrict;
	private String currencyCode;
	private String shortName;
	private String businessTypeCode;
	private String activityText;
	private String federalEmployerId;
	private Integer numberOfEmployees;

}
