package com.polus.formbuilder.programmedelement.opa.entity;

import java.math.BigDecimal;
import java.util.Date;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import jakarta.persistence.Temporal;
import jakarta.persistence.TemporalType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "OPA_DISCL_ACTIVITIES")
public class OPADiscActivityEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "OPA_DISCL_ACTIVITY_ID")
    private Integer opaDisclActivityId;

    @Column(name = "OPA_DISCLOSURE_ID", nullable = false)
    private Integer opaDisclosureId;

    @Column(name = "OPA_DISCL_PERSON_ENTITY_ID")
    private Integer opaDisclPersonEntityId;
	
    @Column(name = "IS_COMPENSATED", length = 1, nullable = false)
    private String isCompensated;

    @Column(name = "NUM_OF_DAYS_SUMMER", precision = 5, scale = 2)
    private BigDecimal numOfDaysSummer;

    @Column(name = "NUM_OF_DAYS_ACADEMIC", precision = 5, scale = 2)
    private BigDecimal numOfDaysAcademic;

    @Column(name = "NUM_OF_DAYS_IN_YEAR", precision = 5, scale = 2)
    private BigDecimal numOfDaysInYear;
    
    @Column(name = "NATURE_OF_WORK", length = 4000)
    private String natureOfWork;
    
    @Column(name = "DESCRIPTION_1", length = 4000)
    private String description1;
    
    @Column(name = "DESCRIPTION_2", length = 4000)
    private String description2;

    @Column(name = "UPDATE_TIMESTAMP", nullable = false)
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER", length = 60, nullable = false)
    private String updateUser;

}
