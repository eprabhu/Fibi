package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.coi.pojo.CoiDisclEntProjDetails;
import com.polus.fibicomp.coi.pojo.CoiDisclosure;
import com.polus.fibicomp.coi.pojo.CoiReview;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentAttachment;
import com.polus.fibicomp.coi.pojo.CoiReviewCommentTag;
import com.polus.fibicomp.coi.pojo.CoiSectionsType;
import com.polus.fibicomp.coi.pojo.PersonEntity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Builder.Default;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiReviewCommentsDto {

	private Integer coiReviewCommentId;
	
	private Integer coiReviewId;
	
	private CoiReview coiReview;

	private String coiSectionsTypeCode;
	
	private CoiSectionsType coiSectionsType;

	private String coiSubSectionsId;

	private String coiReviewActivityId;
	
	private Integer disclosureId;
	
	private CoiDisclosure coiDisclosure;
	
	private String commentedByPersonId;
	
	@Default private Boolean isPrivate = false;

	private Integer coiParentCommentId;
	
	private String comment;

	private String updateUser;
	
	private Timestamp updateTimestamp;

	private List<CoiReviewCommentAttachment> coiReviewCommentAttachment;
	
	private String updateUserFullName;
	
	private List<CoiReviewCommentTag> coiReviewCommentTag;

	private PersonEntity personEntity;

	private CoiDisclEntProjDetails disclEntProjDetails;

	private Integer componentSubRefId;

	private Integer commentId;

}
