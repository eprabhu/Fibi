package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
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
@Table(name = "COI_QUEST_ANSWER")
public class CoiQuestAnswer implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUESTIONNAIRE_ANSWER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questAnswerId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_QUEST_ANSWER_FK1"), name = "QUESTIONNAIRE_ANS_HEADER_ID", referencedColumnName = "QUESTIONNAIRE_ANS_HEADER_ID")
	private QuestAnswerHeader questAnswerHeader;

	@Column(name = "QUESTION_ID")
	private Integer questionId;

	@Column(name = "OPTION_NUMBER")
	private Integer optionNumber;

	@Column(name = "ANSWER_NUMBER")
	private Integer answerNumber;

	@Column(name = "ANSWER")
	private String answer;

	@Column(name = "ANSWER_LOOKUP_CODE")
	private String answerLookUpCode;

	@Column(name = "EXPLANATION")
	private String explanation;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "questAnswer", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<CoiQuestAnswerAttachment> questAnswerAttachment;

}
