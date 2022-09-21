package com.polus.fibicomp.scopusintegration.vo;

import java.util.List;

import com.polus.fibicomp.award.pojo.AwardAcheivements;
import com.polus.fibicomp.award.pojo.AwardAssociation;
import com.polus.fibicomp.award.pojo.AwardAssociationType;
import com.polus.fibicomp.award.pojo.AwardPublications;
import com.polus.fibicomp.scopusintegration.pojo.AwardScopus;
import com.polus.fibicomp.scopusintegration.pojo.Scopus;
import com.polus.fibicomp.scopusintegration.pojo.ScopusAffiliation;
import com.polus.fibicomp.scopusintegration.pojo.ScopusAuthors;
import com.polus.fibicomp.scopusintegration.pojo.ScopusMetrics;

public class ScopusVO {

	private String scopusID;

	private String scopusTitle;

	private String scopusCreator;

	private String scopusPublicationId;

	private String scopusISSN;

	private String scopusDOI;

	private String scopusAggregationType;

	private String scopusCoverDate;

	private String scopusDisplayDate;

	private String scopusPubMedId;

	private String scopusCitationCount;

	private String scopusReference;

	private Scopus scopus;
	
	private Integer authorsCount;

	private List<ScopusAuthors> scopusAuthors;

	private List<ScopusAffiliation> scopusAffiliations;

	private List<ScopusMetrics> scopusMetrics;

	private List<Object> links;

	private List<Object> authors;

	private List<Object> affiliations;

	private List<Results> results;
	
	private AwardScopus awardScopus;
	
	private Integer awardId;
	
	private String awardScopusId;
	
	private String updateUser;
	
	private List<AwardAssociation> awardAssociations;

	private List<AwardAcheivements> awardAcheivements;

	private List<AwardAssociationType> associationTypes;
	
	private List<AwardPublications> awardPublications;
	
	private List<AwardScopus> awardScopuses;

	private List<Scopus> scopuses;

	public String getScopusID() {
		return scopusID;
	}

	public void setScopusID(String scopusID) {
		this.scopusID = scopusID;
	}

	public String getScopusTitle() {
		return scopusTitle;
	}

	public void setScopusTitle(String scopusTitle) {
		this.scopusTitle = scopusTitle;
	}

	public String getScopusCreator() {
		return scopusCreator;
	}

	public void setScopusCreator(String scopusCreator) {
		this.scopusCreator = scopusCreator;
	}

	public String getScopusPublicationId() {
		return scopusPublicationId;
	}

	public void setScopusPublicationId(String scopusPublicationId) {
		this.scopusPublicationId = scopusPublicationId;
	}

	public String getScopusISSN() {
		return scopusISSN;
	}

	public void setScopusISSN(String scopusISSN) {
		this.scopusISSN = scopusISSN;
	}

	public String getScopusDOI() {
		return scopusDOI;
	}

	public void setScopusDOI(String scopusDOI) {
		this.scopusDOI = scopusDOI;
	}

	public String getScopusAggregationType() {
		return scopusAggregationType;
	}

	public void setScopusAggregationType(String scopusAggregationType) {
		this.scopusAggregationType = scopusAggregationType;
	}

	public String getScopusCoverDate() {
		return scopusCoverDate;
	}

	public void setScopusCoverDate(String scopusCoverDate) {
		this.scopusCoverDate = scopusCoverDate;
	}

	public String getScopusDisplayDate() {
		return scopusDisplayDate;
	}

	public void setScopusDisplayDate(String scopusDisplayDate) {
		this.scopusDisplayDate = scopusDisplayDate;
	}

	public String getScopusPubMedId() {
		return scopusPubMedId;
	}

	public void setScopusPubMedId(String scopusPubMedId) {
		this.scopusPubMedId = scopusPubMedId;
	}

	public String getScopusCitationCount() {
		return scopusCitationCount;
	}

	public void setScopusCitationCount(String scopusCitationCount) {
		this.scopusCitationCount = scopusCitationCount;
	}

	public Scopus getScopus() {
		return scopus;
	}

	public void setScopus(Scopus scopus) {
		this.scopus = scopus;
	}

	public List<ScopusAuthors> getScopusAuthors() {
		return scopusAuthors;
	}

	public void setScopusAuthors(List<ScopusAuthors> scopusAuthors) {
		this.scopusAuthors = scopusAuthors;
	}

	public List<ScopusMetrics> getScopusMetrics() {
		return scopusMetrics;
	}

	public void setScopusMetrics(List<ScopusMetrics> scopusMetrics) {
		this.scopusMetrics = scopusMetrics;
	}

	public List<Object> getLinks() {
		return links;
	}

	public void setLinks(List<Object> links) {
		this.links = links;
	}

	public String getScopusReference() {
		return scopusReference;
	}

	public void setScopusReference(String scopusReference) {
		this.scopusReference = scopusReference;
	}

	public List<Object> getAuthors() {
		return authors;
	}

	public void setAuthors(List<Object> authors) {
		this.authors = authors;
	}

	public List<Object> getAffiliations() {
		return affiliations;
	}

	public void setAffiliations(List<Object> affiliations) {
		this.affiliations = affiliations;
	}

	public List<ScopusAffiliation> getScopusAffiliations() {
		return scopusAffiliations;
	}

	public void setScopusAffiliations(List<ScopusAffiliation> scopusAffiliations) {
		this.scopusAffiliations = scopusAffiliations;
	}

	public List<Results> getResults() {
		return results;
	}

	public void setResults(List<Results> results) {
		this.results = results;
	}

	public AwardScopus getAwardScopus() {
		return awardScopus;
	}

	public void setAwardScopus(AwardScopus awardScopus) {
		this.awardScopus = awardScopus;
	}

	public String getAwardScopusId() {
		return awardScopusId;
	}

	public void setAwardScopusId(String awardScopusId) {
		this.awardScopusId = awardScopusId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<AwardAssociation> getAwardAssociations() {
		return awardAssociations;
	}

	public void setAwardAssociations(List<AwardAssociation> awardAssociations) {
		this.awardAssociations = awardAssociations;
	}

	public List<AwardAcheivements> getAwardAcheivements() {
		return awardAcheivements;
	}

	public void setAwardAcheivements(List<AwardAcheivements> awardAcheivements) {
		this.awardAcheivements = awardAcheivements;
	}

	public List<AwardAssociationType> getAssociationTypes() {
		return associationTypes;
	}

	public void setAssociationTypes(List<AwardAssociationType> associationTypes) {
		this.associationTypes = associationTypes;
	}

	public List<AwardPublications> getAwardPublications() {
		return awardPublications;
	}

	public void setAwardPublications(List<AwardPublications> awardPublications) {
		this.awardPublications = awardPublications;
	}

	public List<AwardScopus> getAwardScopuses() {
		return awardScopuses;
	}

	public void setAwardScopuses(List<AwardScopus> awardScopuses) {
		this.awardScopuses = awardScopuses;
	}

	public List<Scopus> getScopuses() {
		return scopuses;
	}

	public void setScopuses(List<Scopus> scopuses) {
		this.scopuses = scopuses;
	}

	public Integer getAuthorsCount() {
		return authorsCount;
	}

	public void setAuthorsCount(Integer authorsCount) {
		this.authorsCount = authorsCount;
	}
}
