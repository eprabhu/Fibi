package com.polus.fibicomp.externalreview.pojo;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW_QUESTIONNAIRE")
public class ExtReviewQuestionnaire implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_QUESTIONNAIRE_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer extReviewQuestionnaireId;

    @Column(name = "EXT_REVIEW_ID")
    private Integer extReviewID;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "EXT_REVIEW_ID", insertable = false, updatable = false)
    private ExternalReview externalReview;

    @Column(name = "QUESTIONNAIRE_NUMBER")
    private Integer questionnaireNumber;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Transient
    private Object questionnaireDetail;

    public Integer getExtReviewQuestionnaireId() {
        return extReviewQuestionnaireId;
    }

    public void setExtReviewQuestionnaireId(Integer extReviewQuestionnaireId) {
        this.extReviewQuestionnaireId = extReviewQuestionnaireId;
    }

    public Integer getExtReviewID() {
        return extReviewID;
    }

    public void setExtReviewID(Integer extReviewID) {
        this.extReviewID = extReviewID;
    }

    public ExternalReview getExternalReview() {
        return externalReview;
    }

    public void setExternalReview(ExternalReview externalReview) {
        this.externalReview = externalReview;
    }

    public Timestamp getUpdateTimestamp() {
        return updateTimestamp;
    }

    public void setUpdateTimestamp(Timestamp updateTimestamp) {
        this.updateTimestamp = updateTimestamp;
    }

    public String getUpdateUser() {
        return updateUser;
    }

    public void setUpdateUser(String updateUser) {
        this.updateUser = updateUser;
    }

    public Object getQuestionnaireDetail() {
        return questionnaireDetail;
    }

    public void setQuestionnaireDetail(Object questionnaireDetail) {
        this.questionnaireDetail = questionnaireDetail;
    }

    public Integer getQuestionnaireNumber() {
        return questionnaireNumber;
    }

    public void setQuestionnaireNumber(Integer questionnaireNumber) {
        this.questionnaireNumber = questionnaireNumber;
    }
}
