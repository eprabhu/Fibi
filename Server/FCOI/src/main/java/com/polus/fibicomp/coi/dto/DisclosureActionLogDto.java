package com.polus.fibicomp.coi.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DisclosureActionLogDto {

	private String actionTypeCode;
	private Integer disclosureId;
	private Integer disclosureNumber;
	private String revisionComment;
	private String oldAdmin;
	private String newAdmin;
	private String reviewername;
	private String comment;
	private String fcoiTypeCode;
	private String message;

}
