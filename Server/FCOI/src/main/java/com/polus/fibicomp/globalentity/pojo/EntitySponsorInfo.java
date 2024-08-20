package com.polus.fibicomp.globalentity.pojo;

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

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Table(name = "ENTITY_SPONSOR_INFO")
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class EntitySponsorInfo implements Serializable {

	private static final long serialVersionUID = 1L;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "ID")
    private int id;

    @Column(name = "ENTITY_ID")
	private int entityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "ENTITY_SPONSOR_INFO_FK1"), name = "ENTITY_ID", referencedColumnName = "ENTITY_ID", insertable = false, updatable = false)
	private GlobalEntity entity;

    @Column(name = "SPONSOR_CODE")
    private String sponsorCode;

    @Column(name = "FEED_STATUS_CODE")
    private String feedStatusCode;

    @Column(name = "SPONSOR_TYPE_CODE")
    private String sponsorTypeCode;

    @Column(name = "ACRONYM")
    private String acronym;

    @Column(name = "DODAC_NUMBER")
    private String dodacNumber;

    @Column(name = "AUDIT_REPORT_SENT_FOR_FY")
    private String auditReportSentForFy;

    @Column(name = "DUNNING_CAMPAIGN_ID")
    private String dunningCampaignId;

    @Column(name = "CUSTOMER_NUMBER")
    private String customerNumber;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATED_BY")
    private String updatedBy;

}
