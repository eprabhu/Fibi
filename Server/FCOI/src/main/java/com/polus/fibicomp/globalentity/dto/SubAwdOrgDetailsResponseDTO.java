package com.polus.fibicomp.globalentity.dto;

import java.util.Date;

import com.polus.fibicomp.globalentity.pojo.EntityOrganizationType;

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
	private Integer id;
	private Integer organizationId;
	private EntityOrganizationType entityOrganizationType;
	private Date samExpirationDate;
	private Date subAwdRiskAssmtDate;

}
