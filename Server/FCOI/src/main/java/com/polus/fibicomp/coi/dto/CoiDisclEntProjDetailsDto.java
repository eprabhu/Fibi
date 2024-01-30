package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;

import com.polus.fibicomp.coi.pojo.CoiProjConflictStatusType;
import com.polus.fibicomp.reviewcomments.pojos.DisclComment;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CoiDisclEntProjDetailsDto {

	private Integer disclosureDetailsId;
	private Integer disclosureId;
	private Integer disclosureNumber;
	private Integer personEntityId;
	private Integer entityId;
	private CoiEntityDto coiEntity;
	private Integer entityNumber;
	private Integer moduleCode;
	private String moduleItemKey;
	private String projectConflictStatusCode;
	private CoiProjConflictStatusType coiProjConflictStatusType;
	private String updateUser;
	private Timestamp updateTimestamp;
	private DisclComment disclComment;
	private PersonEntityRelationshipDto personEntityRelationshipDto;
	private Integer prePersonEntityId;
	private Integer personEntityNumber;

}
