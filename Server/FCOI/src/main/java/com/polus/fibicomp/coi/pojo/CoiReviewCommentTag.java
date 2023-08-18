package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "COI_REVIEW_COMMENT_TAGS")
@EntityListeners(AuditingEntityListener.class)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CoiReviewCommentTag implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_REVIEW_COMMENT_TAGS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiReviewCommentTagsId;

	@Column(name = "COI_REVIEW_COMMENT_ID")
	private Integer coiReviewCommentId;

	@Column(name = "TAG_REF")
	private String tagRef;

	@Column(name = "TAGGED_PERSON_ID")
	private String tagPersonId;

	@Column(name = "TAGGED_GROUP_ID")
	private Integer tagGroupId;
	
	@Column(name = "COI_REVIEW_ID")
	private Integer coiReviewId;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String tagPersonFullName;

	@Transient
	private String tagGroupName;

}
