package com.polus.fibicomp.globalentity.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ActionLogRequestDTO {

	private Integer entityId;
	private Integer entityNumber;
	private String entityName;
	private String updatedBy;
	private String dunsNumber;
	private String tabName;
	private String actionLogCode;

}
