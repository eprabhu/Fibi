package com.polus.fibicomp.externalreview.pojo;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.grantcall.pojo.ScoringCriteria;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW_SCORING_CRITERIA")
public class ExtReviewScoringCriteria implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_SCORING_CRITERIA_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer ExtReviewScoringCriteriaId;

    @Column(name = "EXT_REVIEW_ID")
    private Integer extReviewID;

    @JsonIgnore
    @ManyToOne(cascade = {CascadeType.REFRESH}, fetch = FetchType.LAZY)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_S_C_FK_01"), name = "EXT_REVIEW_ID", insertable = false, updatable = false)
    private ExternalReview externalReview;

    @Column(name = "SCORING_CRITERIA_TYPE_CODE")
    private String scoringCriteriaTypeCode;

    @ManyToOne(cascade = {CascadeType.REFRESH}, fetch = FetchType.EAGER)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_S_C_FK_02"), name = "SCORING_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
    private ScoringCriteria scoringCriteria;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    public ExtReviewScoringCriteria() {
    }

    public ExtReviewScoringCriteria(Integer extReviewID, String scoringCriteriaTypeCode, Timestamp updateTimestamp, String updateUser) {
        this.extReviewID = extReviewID;
        this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
        this.updateTimestamp = updateTimestamp;
        this.updateUser = updateUser;
    }

    public Integer getExtReviewScoringCriteriaId() {
        return ExtReviewScoringCriteriaId;
    }

    public void setExtReviewScoringCriteriaId(Integer extReviewScoringCriteriaId) {
        ExtReviewScoringCriteriaId = extReviewScoringCriteriaId;
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

    public String getScoringCriteriaTypeCode() {
        return scoringCriteriaTypeCode;
    }

    public void setScoringCriteriaTypeCode(String scoringCriteriaTypeCode) {
        this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
    }

    public ScoringCriteria getScoringCriteria() {
        return scoringCriteria;
    }

    public void setScoringCriteria(ScoringCriteria scoringCriteria) {
        this.scoringCriteria = scoringCriteria;
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
}
