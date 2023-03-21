package com.polus.fibicomp.scopusintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "SCOPUS_METRICS")
@EntityListeners(AuditingEntityListener.class)
@IdClass(ScopusMetrics.ScopusMetricsId.class)
public class ScopusMetrics implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SCOPUS_ID")
	private String scopusId;

	@Id
	@Column(name = "YEAR")
	private Integer year;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "SCOPUS_METRICS_FK1"), name = "SCOPUS_ID", referencedColumnName = "SCOPUS_ID", insertable = false, updatable = false)
	private Scopus scopus;

	@Column(name = "METRIC_TYPE")
	private String metricType;

	@Column(name = "VALUE")
	private Integer value;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public String getMetricType() {
		return metricType;
	}

	public void setMetricType(String metricType) {
		this.metricType = metricType;
	}

	public Integer getValue() {
		return value;
	}

	public void setValue(Integer value) {
		this.value = value;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getScopusId() {
		return scopusId;
	}

	public void setScopusId(String scopusId) {
		this.scopusId = scopusId;
	}

	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public static final class ScopusMetricsId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String scopusId;

		private Integer year;

		public String getScopusId() {
			return scopusId;
		}

		public void setScopusId(String scopusId) {
			this.scopusId = scopusId;
		}

		public Integer getYear() {
			return year;
		}

		public void setYear(Integer year) {
			this.year = year;
		}

		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("scopusId", this.scopusId).append("year", this.year).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final ScopusMetricsId rhs = (ScopusMetricsId) other;
			return new EqualsBuilder().append(this.scopusId, rhs.scopusId).append(this.year, rhs.year).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.scopusId).append(this.year).toHashCode();
		}
	}

	public Scopus getScopus() {
		return scopus;
	}

	public void setScopus(Scopus scopus) {
		this.scopus = scopus;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
}
