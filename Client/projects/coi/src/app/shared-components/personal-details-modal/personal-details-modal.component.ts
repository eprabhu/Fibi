import { Component, Input, OnInit, HostListener } from '@angular/core';
import { PersonDetailsModalService } from './person-details-modal.service';
import { Subscription } from 'rxjs';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-personal-details-modal',
    templateUrl: './personal-details-modal.component.html',
    styleUrls: ['./personal-details-modal.component.scss'],
    providers: [PersonDetailsModalService]
})
export class PersonalDetailsModalComponent implements OnInit {

    @Input() personId: string = '';

    currentTab = 'PERSON_DETAILS';
    $subscriptions: Subscription[] = [];
    personValues: any;
    canShowPersonDetails = false;

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            this.commonService.closePersonDetailsModal(false);
        }
    }

    constructor(private _personService: PersonDetailsModalService, public commonService: CommonService) { }

    ngOnInit() {
        if (this.personId) {
            this.getPersonData();
            this.setPersonBtnRights();
        }
    }

    private getPersonData(): void {
        this.$subscriptions.push(this._personService.getPersonData(this.personId)
            .subscribe((data: any) => {
                this.personValues = data;
                document.getElementById('coi-person-view-modal-trigger-btn')?.click();
            }, (_error: any) => {
                this.clearModalDataAndShowToast();
            }));
    }

    private setPersonBtnRights(): void {
        const isLoggedInPerson = this.personId === this.commonService.currentUserDetails.personID;
        this.canShowPersonDetails = (isLoggedInPerson || this.commonService.getAvailableRight('MAINTAIN_PERSON'));
    }

    private clearModalDataAndShowToast(): void {
        this.commonService.modalPersonId = '';
        this.commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
    }

    viewPersonDetails(personTrainingId: string): void {
        this.commonService.redirectToPersonDetails(personTrainingId);
    }

}
