package com.polus.fibicomp.fastintegration.service;

public interface FastTransactionService {

	/**
	 * This method is used to do the data migration for expense
	 */
	public void fastDataMigrationForExpenseTracker();

	/**
	 * This method is used to do the data migration for revenue
	 * @param processingType
	 */
	public void fastIntegrationRevenueTransactionRTProcessing(String processingType);

}
