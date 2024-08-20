package com.polus.fibicomp.globalentity.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ExternalReferenceRequestDTO {

	private Integer entityId;
	private Integer entityExternalMappingId;
	private Integer organizationId;
	private String sponsorCode;

}
