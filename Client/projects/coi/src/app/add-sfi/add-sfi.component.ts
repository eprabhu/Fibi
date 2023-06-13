import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { DATE_PLACEHOLDER } from '../../../../fibi/src/app/app-constants';
import { getEndPointOptionsForEntity, getEndPointOptionsForCountry } from '../../../../fibi/src/app/common/services/end-point.config';
import { hideModal } from '../../../../fibi/src/app/common/utilities/custom-utilities';
import { getDateObjectFromTimeStamp, compareDates } from '../../../../fibi/src/app/common/utilities/date-utilities';
import { environment } from '../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../app-constants';
import { CommonService } from '../common/services/common.service';
import { NavigationService } from '../common/services/navigation.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { CoiEntity, EntityDetails } from '../entity-management/entity-details-interface';


declare var $: any;
export interface EndpointOptions {
	contextField: string;
	formatString: string;
	path: string;
	defaultValue: string;
	params: string;
}
@Component({
	selector: 'app-add-sfi',
	templateUrl: './add-sfi.component.html',
	styleUrls: ['./add-sfi.component.scss']
})
export class AddSfiComponent implements OnInit {
	isSaving = false;
	entityDetails: EntityDetails = new EntityDetails();
	additionalDetails: any = {
		sponsorsResearch: false
	};
	deployMap = environment.deployUrl;
	isAddAttachment = false;
	isAddAssignee = false;
	dateTime: string;
	datePlaceHolder = DATE_PLACEHOLDER;
	isReadMore: false;
	showRelationshipModal = false;
	clearField: any = false;
	EntitySearchOptions: any = {};
	countrySearchOptions: EndpointOptions;
	clearCountryField: any;
	$subscriptions: Subscription[] = [];
	mandatoryList = new Map();
	emailWarningMsg: any;
	sfiLookUpList: any = {};
	isExpandedAdditionalDetails = true;
	isResultFromSearch = false;
	riskLevelLookup = [];
	isEntityManagement = false;
	heading: string;
	buttonName: string;
	btnTitle = '';
	isViewMode: any;
	sfiType: string;

	@Output() emitUpdateEvent = new EventEmitter<number>();
	@Input() changeType = '';
	@Input() disclosureDetails: { disclosureId: any, disclosureNumber: any } = { disclosureId: null, disclosureNumber: null };
	@Input() coiEntityManageId: any = null;
	@Input() isEditEntity = false;
	@Input() isSlider = false;

	constructor(public sfiService: SfiService, private _activatedRoute: ActivatedRoute,
		public _commonService: CommonService, private _router: Router, public _navigationService: NavigationService) { }

	ngOnInit(): void {
		this.isEntityManagement = this._router.url.includes('entity-management') || this.checkIsEntityTypeInURL();
		this.setHeader();
		this.getSFILookup();
        this.setDefaultRiskLevel();
		if (this.coiEntityManageId) {
			this.getEntityDetails();
		}
		this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
		this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
	}

	private checkIsEntityTypeInURL(): boolean {
		let isEntityTypeInURL = false;
		this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
			isEntityTypeInURL = params['type'] ? params['type'] === 'SFI' ? false : true : false;
			this.sfiType = params['type'];
		}));
		return isEntityTypeInURL;
	}

	private getSFILookup(): void {
		this.$subscriptions.push(this.sfiService.addSFILookUp().subscribe((res: any) => {
			this.sfiLookUpList = res;
			this.riskLevelLookup = res.entityRiskCategories;
		}));
	}

	setEntityTypeObj(): void {
		this.entityDetails.coiEntity.entityType = this.sfiLookUpList.entityType.find(ele =>
			this.entityDetails.coiEntity.entityTypeCode === ele.entityTypeCode);
	}

	addEntityToggle(event): void {
		hideModal(event);
	}

	hideRelationshipModal(event): void {
		this.showRelationshipModal = event;
		this.clearSFIFields();
	}

	private createOrUpdateEntitySFI(): void {
		this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
			this.saveAdditionalDetails() : this.saveEntityDetails();
	}

	saveEntityDetails(): void {
		this.entityDetails.coiEntity.entityStatusCode = this.getEntityStatusCode();
		this.$subscriptions.push(this.sfiService.saveOrUpdateCoiEntity(this.entityDetails).subscribe((data: CoiEntity) => {
			this.entityDetails.coiEntity = data;
			this.entityDetails.coiEntity.entityId && !this.isEntityManagement ?
				this.saveAdditionalDetails() : this.updateDetails();
		}, _err => {
			this.isSaving = false;
			this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
		}));
	}

	private getEntityStatusCode() {
		return (this._router.url.includes('entity-management') || this.sfiType === 'ENTITY') ? '1' : '3';
	}

	private checkIfSFIAlreadyAdded(entityId, event): void {
		this.mandatoryList.delete('entityAlreadyAdded');
		this.sfiService.isEntityAdded(entityId).subscribe((isSFIAdded: boolean) => {
			if (isSFIAdded) {
				this.mandatoryList.set('entityAlreadyAdded', 'Selected Entity is already added.');
			} else {	
				this.isResultFromSearch = true;
				this.entityDetails.coiEntity = event;
				if (event.country) {
					this.countrySearchOptions.defaultValue = event.country.countryName;
					this.selectedCountryEvent(event.country);
				}
				this.clearCountryField = new String('false');
			}
		}, err => {
			this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
			this._commonService.showToast(HTTP_ERROR_STATUS, 'Entity selection failed. Please try again');
		});
	}

	updateDetails(): void {
		this.emitUpdateEvent.emit(this.entityDetails.coiEntity.entityId);
		this.sfiService.isShowSfiNavBar = false;
		this.isSaving = false;
		// this commented method need for modify entity
		// if (this.changeType === '2') {
		//   $('#actionConfirmationModal').modal('hide');
		// }
		if (!this.isSlider) {
			this._router.navigate(['/coi/entity-management/entity-details'],{ queryParams: { entityManageId: this.entityDetails.coiEntity.entityId }});
		}
		this._commonService.showToast(HTTP_SUCCESS_STATUS, ` ${this.isEditEntity ? 'Update ' : 'Created '}Entity Successfully completed.`);
	}

	private saveAdditionalDetails(): void {
		this.$subscriptions.push(this.sfiService.createSFI(
			{
				personEntity: {
					entityId: this.entityDetails.coiEntity.entityId,
					entityNumber: this.entityDetails.coiEntity.entityNumber,
					...this.additionalDetails
				},
				...this.disclosureDetails
			}).subscribe((data: any) => {
				this.additionalDetails = data.personEntity;
				this.isSaving = false;
				this.showRelationshipModal = true;
				this.additionalDetails.involvementStartDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementStartDate);
				this.additionalDetails.involvementEndDate = getDateObjectFromTimeStamp(this.additionalDetails.involvementEndDate);
			}, _err => {
				this.isSaving = false;
				this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
			}));
	}

	selectNewEntity(event): void {
		this.entityDetails.coiEntity.entityName = event.searchString;
	}

	selectedEvent(event): void {
		if (event) {
			this.clearField = new String('false');
			this.checkIfSFIAlreadyAdded(event.entityId, event);
		} else {
			this.entityDetails = new EntityDetails();
			this.clearSFIFields();
		}
	}

	selectedCountryEvent(event): void {
		if (event) {
			this.entityDetails.coiEntity.countryCode = event.countryCode;
			this.countrySearchOptions.defaultValue = event.countryName;
		} else {
			this.entityDetails.coiEntity.countryCode = null;
		}
	}

	private clearSFIFields(): void {
		this.entityDetails = new EntityDetails();
		this.additionalDetails = {
			sponsorsResearch: false
		};
		this.clearCountryField = new String('true');
		this.clearField = new String('true');
		this.EntitySearchOptions = getEndPointOptionsForEntity(this._commonService.baseUrl);
		this.countrySearchOptions = getEndPointOptionsForCountry(this._commonService.fibiUrl);
		this.isResultFromSearch = false;
		this.mandatoryList.clear();
	}

	private checkMandatoryFilled(): boolean {
		this.mandatoryList.clear();
		if (!this.entityDetails.coiEntity.entityName) {
			this.mandatoryList.set('entityName', 'Please choose an entity name.');
		}
		this.entityDetailsValidation();
		if (!this.isEntityManagement) {
			if (!this.additionalDetails.involvementStartDate) {
				this.mandatoryList.set('date', 'Please enter a start date.');
			}
			if (!this.additionalDetails.staffInvolvement) {
				this.mandatoryList.set('staff', 'Please enter Relationship with Entity details.');
			}
			if (!this.additionalDetails.studentInvolvement) {
				this.mandatoryList.set('student', 'Please enter Principle Business Area of Entity details.');
			}
			if (!this.additionalDetails.instituteResourceInvolvement) {
				this.mandatoryList.set('resource', 'Please enter Relationship of Entity to your University responsibilities details.');
			}
			this.endDateValidation();
		}
		return this.mandatoryList.size !== 0 || this.emailWarningMsg ? false : true;
	}

	private entityDetailsValidation(): void {
		if (!this.entityDetails.coiEntity.countryCode) {
			this.mandatoryList.set('country', 'Please choose a country.');
		}
		if (!this.entityDetails.coiEntity.entityTypeCode || this.entityDetails.coiEntity.entityTypeCode === 'null') {
			this.mandatoryList.set('entityType', 'Please choose an entity type.');
		}
		if (!this.entityDetails.coiEntity.emailAddress) {
			this.mandatoryList.set('email', 'Please enter a email address.');
		}
		if (!this.entityDetails.coiEntity.address) {
			this.mandatoryList.set('address', 'Please enter an address.');
		}
		if (!this.entityDetails.coiEntity.phone) {
			this.mandatoryList.set('phone', 'Please enter phone number.');
		}
		this.emailValidation();
		if (!this.entityDetails.coiEntity.zipCode) {
			this.mandatoryList.set('zipCode', 'Please enter a zipCode.');
		}
	}

	private emailValidation(): void {
		this.emailWarningMsg = null;
		if (this.entityDetails.coiEntity.emailAddress) {
			this.entityDetails.coiEntity.emailAddress = this.entityDetails.coiEntity.emailAddress.trim();
			if (this.entityDetails.coiEntity.emailAddress !== undefined && this.entityDetails.coiEntity.emailAddress !== '') {
				const email = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)| (".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
				if (!(email.test(String(this.entityDetails.coiEntity.emailAddress).toLowerCase()))) {
					this.emailWarningMsg = 'Please select a valid email address.';
				} else {
					this.emailWarningMsg = null;
				}
			}
		}
	}

	inputRestriction(event: any) {
		const pattern = /[0-9\+\-\/\ ]/;
		if (!pattern.test(String.fromCharCode(event.charCode))) {
			event.preventDefault();
		}
	}

	phoneNumberValidation(input) {
		this.mandatoryList.delete('phoneNumber');
		// tslint:disable-next-line:max-line-length
		const pattern = (/^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[0-9]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$/);
		if (!pattern.test(input)) {
			this.checkForInvalidPhoneNumber(input);
		}
	}

	private checkForInvalidPhoneNumber(input): void {
		if (/^([a-zA-Z]|[0-9a-zA-Z])+$/.test(input)) {
			this.mandatoryList.set('phoneNumber', 'Alphabets cannot be added in Phone number field.');
		} else {
			this.mandatoryList.set('phoneNumber', 'Please add a valid number');
		}
	}

	endDateValidation(): void {
		this.mandatoryList.delete('endDate');
		if (this.additionalDetails.involvementStartDate && this.additionalDetails.involvementEndDate &&
			(compareDates(this.additionalDetails.involvementStartDate, this.additionalDetails.involvementEndDate) === 1)) {
			this.mandatoryList.set('endDate', 'Please provide a valid date.');
		}
	}

	setHeader(): void {
		if (this.isEntityManagement) {
			if (this.isEditEntity) {
				this.buttonName = 'Update Entity';
				this.btnTitle = 'Click here to update an entity';
				this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
			} else {
				this.heading = 'Add New Entity';
				this.buttonName = 'Create Entity';
				this.btnTitle = 'Click here to create an entity';
			}
		} else {
			this.heading = 'Significant Financial Interest';
			this.buttonName = 'Create SFI';
			this.btnTitle = 'Click here to create a sfi';
		}
	}

	private getEntityDetails(): void {
		this.$subscriptions.push(this.sfiService.getEntityDetails(this.coiEntityManageId).subscribe((res: EntityDetails) => {
			this.entityDetails = res;
			this.heading = `Entity ${this.entityDetails.coiEntity.entityName}`;
			this.clearCountryField = new String('false');
			this.countrySearchOptions.defaultValue = this.entityDetails.coiEntity.country.countryName;
			this.selectedCountryEvent(res.coiEntity.country);
		}));
	}

	setEntityRiskCategoryObj(): void {
		this.entityDetails.coiEntity.entityRiskCategory = this.riskLevelLookup.find(ele =>
			this.entityDetails.coiEntity.riskCategoryCode === ele.riskCategoryCode);
	}

	submitEntity(): void {
		if (this.mandatoryList.has('entityAlreadyAdded') || (!this.checkMandatoryFilled() && !this.isSaving)) {
			return;
		}
		
		this.createOrUpdateEntitySFI();
		// this commented method need for modify entity
		// this.changeType === '2' ? $('#actionConfirmationModal').modal('show') : this.createOrUpdateEntitySFI();
	}
	
	updateChanges(event): void {
		if (event) {
			this.createOrUpdateEntitySFI();
		}
	}

	backToPreviousPage(): void {
		if (this._navigationService.previousURL) {
			this._router.navigateByUrl(this._navigationService.previousURL);
		} else {
			this._router.navigate(['/coi/user-dashboard']);
		}
	}

    private setDefaultRiskLevel(): void {
        this.entityDetails.coiEntity.riskCategoryCode = '3';
    }

}
