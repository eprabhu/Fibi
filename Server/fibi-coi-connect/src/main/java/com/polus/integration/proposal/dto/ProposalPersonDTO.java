package com.polus.integration.proposal.dto;

import java.math.BigDecimal;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProposalPersonDTO {

	private Integer proposalNumber;

	private String keyPersonName;

	private String keyPersonId;

	private String keyPersonRoleCode;

	private String keyPersonRole;

	private BigDecimal percentageOfEffort;

	private String attribute1Label;

	private String attribute1Value;

	private String attribute2Label;

	private String attribute2Value;

	private String attribute3Label;

	private String attribute3Value;
	
}