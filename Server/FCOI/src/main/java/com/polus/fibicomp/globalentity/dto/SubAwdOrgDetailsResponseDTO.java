package com.polus.fibicomp.globalentity.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SubAwdOrgDetailsResponseDTO {

	private Integer entityId;
	private Integer organizationId;
	private String organizationTypeCode;
	private Date samExpirationDate;
	private Date subAwdRiskAssmtDate;

}
