package com.polus.formbuilder.reviewcomments.modals;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Transient;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

@NoArgsConstructor
@AllArgsConstructor
@Builder
@Getter
@Setter
public class ReviewComment {

	private Integer commentId;
	private String documentOwnerPersonId;
	private String commentPersonId;
	private String commentTypeCode;
	private String componentTypeCode;
	private Integer parentCommentId;
	private List<ReviewCommentTag> commentTags;
	private Boolean isPrivate;
	private String comment;
	private Integer moduleItemKey;
	private String moduleItemNumber;
	private Integer subModuleItemKey;
	private String subModuleItemNumber;
	private Integer moduleCode;
	private Integer subModuleCode;
	private Integer formBuilderId;
	private Integer formBuilderSectionId;
	private Integer formBuilderComponentId;
	private String updateUser;
	private Timestamp updateTimestamp;
	private String updateUserFullName;
	private List<ReviewComment> childComments;
	private List<Attachments> attachments;
	private Boolean isSectionDetailsNeeded;
}
