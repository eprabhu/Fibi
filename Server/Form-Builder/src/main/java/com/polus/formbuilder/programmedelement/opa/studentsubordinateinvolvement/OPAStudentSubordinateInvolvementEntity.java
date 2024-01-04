package com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement;

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
@Table(name = "OPA_STUDENT_SUBORDINATE_INVOLVEMENT")
public class OPAStudentSubordinateInvolvementEntity {

	@Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "OPA_STUD_SUB_INV_ID")
    private Integer opaStudSubInvId;

    @Column(name = "OPA_DISCLOSURE_ID")
    private Integer opaDisclosureId;

    @Column(name = "OPA_PERSON_TYPE_CODE")
    private String opaPersonTypeCode;

    @Column(name = "PERSON_ID")
    private String personId;

    @Column(name = "NATURE_OF_WORK")
    private String natureOfWork;

    @Column(name = "DESCRIPTION_1")
    private String description1;
    
    @Column(name = "DESCRIPTION_2")
    private String description2;

    @Column(name = "OPA_DISCL_PERSON_ENTITY_ID")
    private Integer opaDisclPersonEntityId;

    @Column(name = "RELATIONSHIP")
    private String relationWithPerson;

    @Column(name = "NUM_OF_DAYS", precision = 5, scale = 2)
    private BigDecimal numOfDays;

    @Column(name = "UPDATE_TIMESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

}
