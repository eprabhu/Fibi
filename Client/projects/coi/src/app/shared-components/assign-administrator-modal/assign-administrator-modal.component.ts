import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { CompleterOptions } from '../../../../../fibi/src/app/service-request/service-request.interface';
import { CommonService } from '../../common/services/common.service';
import { Subscription } from 'rxjs';
import { AssignAdministratorModalService } from './assign-administrator-modal.service';
import { DefaultAdminDetails } from '../../travel-disclosure/travel-disclosure-interface';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { AssignAdminRO } from '../shared-interface';

declare const $: any;

@Component({
    selector: 'app-assign-administrator-modal',
    templateUrl: './assign-administrator-modal.component.html',
    styleUrls: ['./assign-administrator-modal.component.scss'],
    providers: [AssignAdministratorModalService]
})
export class AssignAdministratorModalComponent implements OnInit, OnChanges, OnDestroy {

    isAssignToMe = false;
    adminSearchOptions: any = {};
    clearAdministratorField: String;
    assignAdminMap = new Map();
    addAdmin = new AssignAdminRO();
    adminGroupsCompleterOptions: CompleterOptions = new CompleterOptions();
    clearAdminGroupField: any;
    $subscriptions: Subscription[] = [];
    isSaving = false;

    @Input() disclosureId = null;
    @Input() defaultAdminDetails = new DefaultAdminDetails();
    @Input() path: 'DISCLOSURES' | 'TRAVEL_DISCLOSURES' = 'DISCLOSURES';
    @Output() closeModal: EventEmitter<any> = new EventEmitter<any>();

    constructor(private _commonService: CommonService, private _assignAdminService: AssignAdministratorModalService) { }

    ngOnInit(): void {
        document.getElementById('toggle-assign-admin').click();
        if (this.checkDefaultAdminPersonId()) {
            document.getElementById('assignCheck').click();
        }
        this.setDefaultAdminDetails();
    }

    ngOnChanges(): void {
        this.getAdminDetails();
        this.setDisclosureId();
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    private getAdminDetails() {
        this.$subscriptions.push(this._assignAdminService.getAdminDetails().subscribe((data: any) => {
            this.setAdminGroupOptions(data);
            this.setCompleterOptions(data.persons, 'fullName', this.adminSearchOptions);
        }));
    }

    private checkDefaultAdminPersonId(): boolean {
        return this.defaultAdminDetails.adminPersonId === this._commonService.getCurrentUserDetail('personId');
    }

    private setDefaultAdminDetails(): void {
        this.addAdmin.adminGroupId = this.defaultAdminDetails.adminGroupId;
        this.addAdmin.adminPersonId = this.defaultAdminDetails.adminPersonId;
        this.adminSearchOptions.defaultValue = this.defaultAdminDetails.adminPersonName;
    }

    private setDisclosureId(): void {
        if (this.path === 'TRAVEL_DISCLOSURES') {
            this.addAdmin.travelDisclosureId = this.disclosureId;
        } else {
            this.addAdmin.disclosureId = this.disclosureId;
        }
    }

    private setAdminGroupOptions(data): void {
        this.adminGroupsCompleterOptions = {
            arrayList: this.getActiveAdminGroups(data),
            contextField: 'adminGroupName',
            filterFields: 'adminGroupName',
            formatString: 'adminGroupName',
            defaultValue: this.defaultAdminDetails.adminGroupName
        };
    }

    private getActiveAdminGroups(data) {
        return data.adminGroups.filter(element => element.isActive === 'Y');
    }

    private setCompleterOptions(arrayList: any, searchShowField: string, searchOption: any = null) {
        searchOption.defaultValue = '';
        searchOption.arrayList = arrayList || [];
        searchOption.contextField = searchShowField;
        searchOption.filterFields = searchShowField;
        searchOption.formatString = searchShowField;
    }
    public assignToMe(checkBoxEvent: any) {
        if (checkBoxEvent.target.checked) {
            this.adminSearchOptions.defaultValue = this._commonService.getCurrentUserDetail('fullName');
            this.addAdmin.adminPersonId = this._commonService.getCurrentUserDetail('personId');
            this.isAssignToMe = true;
            this.assignAdminMap.clear();
        } else {
            this.addAdmin.adminPersonId = this.checkDefaultAdminPersonId() ? null : this.defaultAdminDetails.adminPersonId;
            this.adminSearchOptions.defaultValue = this.checkDefaultAdminPersonId() ? '' : this.defaultAdminDetails.adminPersonName;
            this.isAssignToMe = false;
        }
        this.clearAdministratorField = new String('false');
    }

    public adminSelect(event: any) {
        if (event) {
            this.addAdmin.adminPersonId = event.personId;
            this.isAssignToMe = this.setAssignToMe();
            this.assignAdminMap.clear();
        } else {
            this.addAdmin.adminPersonId = null;
            this.isAssignToMe = false;
        }
    }

    public adminGroupSelect(event) {
        this.addAdmin.adminGroupId = (event?.adminGroupId) || null;
    }

    public assignAdministrator() {
        if (!this.isSaving && this.validateAdmin()) {
            this.isSaving = true;
            this.setDisclosureId();
            const path = this.path === 'DISCLOSURES' ? 'disclosure' : 'travelDisclosure';
            this.$subscriptions.push(this._assignAdminService.assignAdmin(path, this.addAdmin)
                .subscribe((data: any) => {
                    this.isAssignToMe = false;
                    this.addAdmin = new AssignAdminRO();
                    this.isSaving = false;
                    this.clearAdministratorField = new String('true');
                    this.closeModal.emit(data);
                    document.getElementById('toggle-assign-admin').click();
                }, err => {
                    this.isSaving = false;
                }));
        }
    }

    private validateAdmin(): boolean {
        this.assignAdminMap.clear();
        if (!this.addAdmin.adminPersonId) {
            this.assignAdminMap.set('adminName', 'adminName');
        }
        return this.assignAdminMap.size > 0 ? false : true;
    }

    private setAssignToMe(): boolean {
        return this.addAdmin.adminPersonId === this._commonService.getCurrentUserDetail('personId') ? true : false;
    }

    public clearData() {
        this.isAssignToMe = false;
        this.closeModal.emit();
        this.addAdmin = new AssignAdminRO();
        this.clearAdminGroupField = new String('true');
        this.clearAdministratorField = new String('true');
    }
}
