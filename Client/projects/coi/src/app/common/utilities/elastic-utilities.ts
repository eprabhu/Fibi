export function setEntityObjectFromElasticResult(data: any) {
    return {
        entityId: data.entity_id,
        entityNumber: data.entity_number,
        entityName: data.entity_name,
        versionNumber: data.version_number,
        versionStatus: data.version_status,
        entityStatusCode: data.entity_status_code,
        entityStatus: {
            entityStatusCode: data.entity_status_code,
            description: data.entity_status
        },
        entityTypeCode: data.entity_type_code,
        entityType: {
            entityTypeCode: data.entity_type_code,
            description: data.entity_type
        },
        riskCategoryCode: data.risk_category_code,
        entityRiskCategory: {
            riskCategoryCode: data.risk_category_code,
            description: data.risk_category
        },
        phone: data.phone,
        countryCode: data.country_code,
        country: {
            countryCode: data.country_code,
            countryName: data.country_name
        },
        city: data.city,
        address: data.address,
        zipCode: data.zip_code,
        emailAddress: data.email_address,
        isActive: data.is_active,
        webURL: data.web_url,
        createUser: data.create_user,
        createTimestamp: data.create_timestamp,
        updateUser: data.update_user,
        updateTimestamp: data.update_timestamp,
        approvedUser: data.approved_user,
        approvedTimestamp: data.approved_timestamp,
        revisionReason: data.revision_reason,
        updatedUserFullName: data.update_user_full_name,
        createUserFullName: data.create_user_full_name
    }
}
