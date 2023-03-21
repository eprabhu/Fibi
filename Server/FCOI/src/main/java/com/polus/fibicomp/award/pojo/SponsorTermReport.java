package com.polus.fibicomp.award.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;

@Entity
@Table(name = "SPONSOR_TERM_REPORT")
public class SponsorTermReport implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SPONSOR_TERM_REPORT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_SPONSOR_TERM_REPORT")
	@SequenceGenerator(name="SEQ_SPONSOR_TERM_REPORT", sequenceName = "SEQ_SPONSOR_TERM_REPORT", allocationSize=1)
	private Integer sponsorTermReportId;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_TERM_REPORT_ID_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "SPONSOR_TERM_ID")
	private Integer sponsorTermId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_TERM_REPORT_ID_FK3"), name = "SPONSOR_TERM_ID", referencedColumnName = "SPONSOR_TERM_ID", insertable = false, updatable = false)
	private SponsorTerm sponsorTerm;

	@Column(name = "FUNDING_SCHEME_ID")
	private Integer fundingSchemeId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_TERM_REPORT_ID_FK2"), name = "FUNDING_SCHEME_ID", referencedColumnName = "FUNDING_SCHEME_ID", insertable = false, updatable = false)
	private SponsorFundingScheme sponsorFundingScheme;

	public Integer getSponsorTermReportId() {
		return sponsorTermReportId;
	}

	public void setSponsorTermReportId(Integer sponsorTermReportId) {
		this.sponsorTermReportId = sponsorTermReportId;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public SponsorTerm getSponsorTerm() {
		return sponsorTerm;
	}

	public void setSponsorTerm(SponsorTerm sponsorTerm) {
		this.sponsorTerm = sponsorTerm;
	}

	public Integer getSponsorTermId() {
		return sponsorTermId;
	}

	public void setSponsorTermId(Integer sponsorTermId) {
		this.sponsorTermId = sponsorTermId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getFundingSchemeId() {
		return fundingSchemeId;
	}

	public void setFundingSchemeId(Integer fundingSchemeId) {
		this.fundingSchemeId = fundingSchemeId;
	}

	public SponsorFundingScheme getSponsorFundingScheme() {
		return sponsorFundingScheme;
	}

	public void setSponsorFundingScheme(SponsorFundingScheme sponsorFundingScheme) {
		this.sponsorFundingScheme = sponsorFundingScheme;
	}

}
