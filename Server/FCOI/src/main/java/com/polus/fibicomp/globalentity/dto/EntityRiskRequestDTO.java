package com.polus.fibicomp.globalentity.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntityRiskRequestDTO {

	private Integer entityId;
	private Integer entityRiskId;
	private String riskTypeCode;
	private String riskLevelCode;
	private String description;

}
