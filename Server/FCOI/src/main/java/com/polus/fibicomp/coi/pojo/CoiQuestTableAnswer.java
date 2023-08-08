package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.questionnaire.pojo.QuestAnswerHeader;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "COI_QUEST_TABLE_ANSWER")
public class CoiQuestTableAnswer implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUEST_TABLE_ANSWER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questTableAnswerId;

	@Column(name = "QUESTIONNAIRE_ANS_HEADER_ID")
	private Integer questAnsHeaderId;

	@JsonBackReference
	@ManyToOne(optional = true, cascade = CascadeType.REFRESH, fetch = FetchType.LAZY)
	@JoinColumn(name = "QUESTIONNAIRE_ANS_HEADER_ID", insertable = false, updatable = false)
	private QuestAnswerHeader questAnswerHeader;

	@Column(name = "QUESTION_ID")
	private Integer questionId;

	@Column(name = "ORDER_NUMBER")
	private Integer orderNumber;

	@Column(name = "COLUMN_1")
	private String column1;

	@Column(name = "COLUMN_2")
	private String column2;

	@Column(name = "COLUMN_3")
	private String column3;

	@Column(name = "COLUMN_4")
	private String column4;

	@Column(name = "COLUMN_5")
	private String column5;

	@Column(name = "COLUMN_6")
	private String column6;

	@Column(name = "COLUMN_7")
	private String column7;

	@Column(name = "COLUMN_8")
	private String column8;

	@Column(name = "COLUMN_9")
	private String column9;

	@Column(name = "COLUMN_10")
	private String column10;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

}
