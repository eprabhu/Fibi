package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "COI_QUEST_ANSWER_ATTACHMENT")
public class CoiQuestAnswerAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUESTIONNAIRE_ANSWER_ATT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questAnswerAttachId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_QUEST_ANSWER_ATTACHMENT_FK1"), name = "QUESTIONNAIRE_ANSWER_ID", referencedColumnName = "QUESTIONNAIRE_ANSWER_ID")
	private CoiQuestAnswer questAnswer;

	@Column(name = "ATTACHMENT")
	private byte[] attachment;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

}
