package com.polus.fibicomp.report.repository;

import com.polus.fibicomp.report.pojo.AuditReportType;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface AuditReportTypeRepository extends CrudRepository<AuditReportType, Integer> {

    /**
     * This method is to get AuditReportType by Report Type
     *
     * @param reportType
     * @return AuditReportType
     */
    AuditReportType findAuditReportTypeByReportType(String reportType);
}
