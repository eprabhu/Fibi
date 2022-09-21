import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { ElasticConfigService } from '../../../common/services/elastic-config.service';
import { getEndPointOptionsForDepartment } from '../../../common/services/end-point.config';
import { concatUnitNumberAndUnitName, deepCloneObject } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { PersonDetails } from '../common/role-maintenance';
import { RoleMaintainanceService } from '../role-maintainance.service';
import { UserRoleService } from './user-role.service';

declare var $: any;

class RoleDetails {
	unitNumber = null;
	personId = null;
}

@Component({
	selector: 'app-user-role-creation',
	templateUrl: './user-role-creation.component.html',
	styleUrls: ['./user-role-creation.component.css'],
	providers: [UserRoleService]

})
export class UserRoleCreationComponent implements OnInit, OnDestroy {

	elasticSearchOptions: any = {};
	unitSearchOptions: any = {};
	personUnitName: any;
	roleDetails: RoleDetails = new RoleDetails();
	isShowPersonCard = false;
	isAssignedAndUnassignedWidget = false;
	$subscriptions: Subscription[] = [];
	unAssignedList: any;
	tempUnAssignedList: any;
	assignedList: any;
	tempAssignedList: any;
	tempModalDataList: any;
	isSaving = false;
	assignedDetails: any = {
		personId: '',
		unitNumber: '',
		updateUser: '',
		roles: []
	};
	searchText: any;
	search: any;
	isPersonFieldDisabled = false;
	isRoleUnitDisable = false;
	clickedOption: any;
	personDetails = new PersonDetails();
	clearField: boolean;

	constructor(private _elasticConfig: ElasticConfigService,
		private _userRoleService: UserRoleService,
		private _commonService: CommonService,
		private _route: ActivatedRoute,
		public roleMaintenanceService: RoleMaintainanceService) { }

	ngOnInit() {
		this.elasticSearchOptions = this._elasticConfig.getElasticForPerson();
		this.unitSearchOptions = getEndPointOptionsForDepartment();
		this.assignedDetails.updateUser = this._commonService.getCurrentUserDetail('userName');
		this.checkForEditAndAssign();
		this.assignedAndUnAssignedRolesForPerson();
	}

	private checkForEditAndAssign(): void {
		this.clickedOption = this._route.snapshot.queryParams['CO'];
		this.clickedOption === 'E'
			? this.setDefaultValues(this.roleMaintenanceService.editPersonDetails, this.roleMaintenanceService.editUnitDetails)
			: this.setDefaultValues(this.roleMaintenanceService.assignPersonDetails, this.roleMaintenanceService.assignUnitDetails);
	}

	private setDefaultValues(personObj, unitObj): void {
		if (personObj.personId) {
			this.elasticSearchOptions.defaultValue = personObj.personName;
			this.roleDetails.personId = personObj.personId;
			this.isShowPersonCard = true;
			this.isPersonFieldDisabled = true;
			this.personDetails = Object.assign(personObj);
		}
		if (unitObj.unitNumber) {
			this.unitSearchOptions.defaultValue = concatUnitNumberAndUnitName(unitObj.unitNumber, unitObj.unitName);
			this.roleDetails.unitNumber = unitObj.unitNumber;
			this.isAssignedAndUnassignedWidget = true;
			this.isRoleUnitDisable = true;
		}
	}

	selectUserElasticResult(result): void {
		result ? this.setSearchResults('person', result) : this.setBasicDetailsOfSelectedPersonNull();
	}

	selectUnitSearchValues(result): void {
		if (result) {
			this.setSearchResults('unit', result);
		} else {
			this.personUnitName = null;
		}
	}

	selectAllUnAssigned(check): void {
		this.tempUnAssignedList.forEach(element => {
		  element.selected = check;
		});
	  }

	selectAllAssigned(check): void {
	this.tempAssignedList.forEach(element => {
		element.selected = check;
	});
	}

	private setSearchResults(type, result): void {
		type === 'person'
		? this.setSelectedPerson(this.roleMaintenanceService.assignPersonDetails, result)
		: this.unitChangeFunction(this.roleMaintenanceService.assignUnitDetails, result);
	}

	private setSelectedPerson(personDetails, result): void {
		this.personDetails.personName = personDetails.personName = result.full_name;
		this.personDetails.email_id = personDetails.email_id = result.email_addr;
		this.personDetails.personId = personDetails.personId = result.prncpl_id;
		this.personDetails.unit_name = personDetails.unit_name = result.unit_name;
		this.personDetails.user_name = personDetails.user_name = result.prncpl_nm;
		this.personDetails.primaryTitle = personDetails.primaryTitle = result.primaryTitle;
		this.personDetails.directoryTitle = personDetails.directoryTitle = result.directory_title;
		this.personDetails.isExternalUser = personDetails.isExternalUser = result.external == 'Y' ? true : false;
		this.roleDetails.personId = this.assignedDetails.personId = result.prncpl_id;
		this.isShowPersonCard = true;
		this.assignedAndUnAssignedRolesForPerson();
	}

	private unitChangeFunction(unitObj, event): void {
		if (event) {
			this.personUnitName = unitObj.unitName = event.unitName;
			this.roleDetails.unitNumber = unitObj.unitNumber = event.unitNumber;
			this.assignedAndUnAssignedRolesForPerson();
		}
	}

	private setBasicDetailsOfSelectedPersonNull(): void {
		this.roleMaintenanceService.editPersonDetails = new PersonDetails();
		this.roleDetails.personId = this.assignedDetails.personId = '';
	}

	setDescentType(list): void {
		this.assignedDetails.roles = [];
		list.acType = 'U';
		list.descentFlag = list.descentFlag === 'N' ? 'N' : 'Y'; // changed
		delete list.selected;
		this.assignedDetails.roles.push(list);
		this.$subscriptions.push(this._userRoleService.assignRoles(this.assignedDetails).subscribe());
	}

	private assignedAndUnAssignedRolesForPerson(): void {
		if (this.roleDetails.personId !== null && this.roleDetails.unitNumber !== null) {
			this.assignedDetails.personId = this.roleDetails.personId;
			this.assignedDetails.unitNumber = this.roleDetails.unitNumber;
			this.isAssignedAndUnassignedWidget = true;
			this.$subscriptions.push(this._userRoleService.getUnAssignedList(this.roleDetails).subscribe(
				(data: any) => {
					this.unAssignedList = data;
					this.tempUnAssignedList = this.unAssignedList;
				}));
			this.$subscriptions.push(this._userRoleService.getAssignedList(this.roleDetails).subscribe(
				(data: any) => {
					this.assignedList = data;
					this.tempAssignedList = this.assignedList;
				}));
		}
	}

	noUnassignedData(): void {
		this.tempModalDataList = this.unAssignedList.filter(list => list.selected === true);
		$('#UnassignedRoleModal').modal('show');
	}

	noAssignedData(): void {
		this.tempModalDataList = this.assignedList.filter(list => list.selected === true);
		$('#assignedRoleModal').modal('show');
	}

	moveUnAssignedList(): void {
		if (!this.isSaving) {
			this.isSaving = true;
			const SELECTED_LIST = deepCloneObject(this.unAssignedList.filter(e => e.selected)); // changed
			SELECTED_LIST.forEach((e: any) => {
				delete e.selected;
				e.acType = 'I';
			});
			this.assignedDetails.roles = SELECTED_LIST;
			this.assignedDetails.unitNumber = this.roleDetails.unitNumber;
			this.$subscriptions.push(this._userRoleService.assignRoles(this.assignedDetails).subscribe(
				(data: any) => {
					this.tempAssignedList = this.assignedList.concat(data);
					this.assignedList = this.tempAssignedList;
					this.isSaving = false;
					this.unAssignedList = this.unAssignedList.filter(e => !e.selected);
					this.tempUnAssignedList = this.unAssignedList;
					this.searchText = null;
					this.search = null;
				}, err => { this.isSaving = false; }));
		}
	}

	moveAssignedList(): void {
		if (!this.isSaving) {
			this.isSaving = true;
			const SELECTED_LIST = deepCloneObject(this.assignedList.filter(e => e.selected)); // changed
			SELECTED_LIST.forEach((e: any) => {
				delete e.selected;
				e.acType = 'D';
			});
			this.assignedDetails.roles = SELECTED_LIST;
			this.$subscriptions.push(this._userRoleService.assignRoles(this.assignedDetails).subscribe(
				(data: any) => {
					this.tempUnAssignedList = this.unAssignedList.concat(data);
					this.unAssignedList = this.tempUnAssignedList;
					this.isSaving = false;
					this.assignedList = this.assignedList.filter(e => !e.selected);
					this.tempAssignedList = this.assignedList;
					this.searchText = null;
					this.search = null;
				}, err => { this.isSaving = false; }));
		}
	}

	filterAssignedUnits(searchText): void {
		if (searchText !== '') {
			this.tempAssignedList = this.assignedList.filter(v => {
				return v.roleName.toLowerCase().includes(searchText.toLowerCase());
			});
		} else {
			this.tempAssignedList = this.assignedList;
		}
	}

	filterUnassignedUnits(searchText): void {
		if (searchText !== '') {
			this.tempUnAssignedList = this.unAssignedList.filter(v => {
				return v.roleName.toLowerCase().includes(searchText.toLowerCase());
			});
		} else {
			this.tempUnAssignedList = this.unAssignedList;
		}
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

}
