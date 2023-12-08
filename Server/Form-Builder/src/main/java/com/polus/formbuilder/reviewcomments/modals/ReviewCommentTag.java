package com.polus.formbuilder.reviewcomments.modals;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import java.sql.Timestamp;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ReviewCommentTag {

	private Integer reviewCommentTagId;
	private Integer commentId;
	private String tagRef;
	private String tagPersonId;
	private Integer tagGroupId;
	private Timestamp updateTimestamp;
	private String updateUser;
	private String tagPersonFullName;
	private String tagGroupName;

}
