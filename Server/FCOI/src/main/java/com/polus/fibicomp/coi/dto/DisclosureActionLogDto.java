package com.polus.fibicomp.coi.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

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
	private String riskCategoryCode;
	private String riskCategory;
	private String newRiskCategoryCode;
	private String newRiskCategory;
	private List<String> actionTypeCodes;
	private String administratorName;
	private String oldReviewer;
	private String newReviewer;
	private String reporter;
	private String coiAdmin;	

}
