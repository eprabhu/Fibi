package com.polus.kcintegration.award.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AwardDTO {

	private List<String> linkedInstProposalNumbers;

	private String projectId;

	private String projectNumber;

	private String parentProjectNumber;

	private String rootProjectNumber;

	private String accountNumber;

	private Integer versionNumber;

	private String sponsorGrantNumber;

	private String leadUnitNumber;

	private String leadUnitName;

	private Timestamp projectStartDate;

	private Timestamp projectEndDate;

	private String projectTypeCode;

	private String projectType;

	private String title;

	private String projectStatusCode;

	private String projectStatus;

	private String sponsorCode;

	private String sponsorName;

	private String primeSponsorCode;

	private String primeSponsorName;

	private BigDecimal anticipatedTotal;

	private BigDecimal obligatedTotal;

	private String documentUrl;

	private Timestamp srcSysUpdateTimestamp;

	private String srcSysUpdatedBy;

	private String attribute1Label;

	private String attribute1Value;

	private String attribute2Label;

	private String attribute2Value;

	private String attribute3Label;

	private String attribute3Value;

	private String attribute4Label;

	private String attribute4Value;

	private String attribute5Label;

	private String attribute5Value;

	private List<AwardPersonDTO> projectPersons;

}
