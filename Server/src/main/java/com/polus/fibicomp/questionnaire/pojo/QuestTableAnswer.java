package com.polus.fibicomp.questionnaire.pojo;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

@Entity
@Table(name = "QUEST_TABLE_ANSWER")
public class QuestTableAnswer implements Serializable {

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

	public Integer getQuestTableAnswerId() {
		return questTableAnswerId;
	}

	public void setQuestTableAnswerId(Integer questTableAnswerId) {
		this.questTableAnswerId = questTableAnswerId;
	}

	public Integer getQuestAnsHeaderId() {
		return questAnsHeaderId;
	}

	public void setQuestAnsHeaderId(Integer questAnsHeaderId) {
		this.questAnsHeaderId = questAnsHeaderId;
	}

	public QuestAnswerHeader getQuestAnswerHeader() {
		return questAnswerHeader;
	}

	public void setQuestAnswerHeader(QuestAnswerHeader questAnswerHeader) {
		this.questAnswerHeader = questAnswerHeader;
	}

	public Integer getQuestionId() {
		return questionId;
	}

	public void setQuestionId(Integer questionId) {
		this.questionId = questionId;
	}

	public Integer getOrderNumber() {
		return orderNumber;
	}

	public void setOrderNumber(Integer orderNumber) {
		this.orderNumber = orderNumber;
	}

	public String getColumn1() {
		return column1;
	}

	public void setColumn1(String column1) {
		this.column1 = column1;
	}

	public String getColumn2() {
		return column2;
	}

	public void setColumn2(String column2) {
		this.column2 = column2;
	}

	public String getColumn3() {
		return column3;
	}

	public void setColumn3(String column3) {
		this.column3 = column3;
	}

	public String getColumn4() {
		return column4;
	}

	public void setColumn4(String column4) {
		this.column4 = column4;
	}

	public String getColumn5() {
		return column5;
	}

	public void setColumn5(String column5) {
		this.column5 = column5;
	}

	public String getColumn6() {
		return column6;
	}

	public void setColumn6(String column6) {
		this.column6 = column6;
	}

	public String getColumn7() {
		return column7;
	}

	public void setColumn7(String column7) {
		this.column7 = column7;
	}

	public String getColumn8() {
		return column8;
	}

	public void setColumn8(String column8) {
		this.column8 = column8;
	}

	public String getColumn9() {
		return column9;
	}

	public void setColumn9(String column9) {
		this.column9 = column9;
	}

	public String getColumn10() {
		return column10;
	}

	public void setColumn10(String column10) {
		this.column10 = column10;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
}
