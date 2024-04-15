import { Component, Output, Input, OnInit, EventEmitter, HostListener } from '@angular/core';
import { PersonDetailsModalService } from './person-details-modal.service';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-personal-details-modal',
    templateUrl: './personal-details-modal.component.html',
    styleUrls: ['./personal-details-modal.component.scss'],
    providers: [PersonDetailsModalService]
})
export class PersonalDetailsModalComponent implements OnInit {

    constructor(private _personService: PersonDetailsModalService, private _commonService: CommonService) { }

    @Input() userdetails: any;
    @Output() closeModalPersonDetails: EventEmitter<boolean> = new EventEmitter<boolean>();

    currentTab = 'PERSON_DETAILS';
    $subscriptions: Subscription[] = [];
    personValues: any;
    canShowPersonDetails = false;

    ngOnInit() {
        document.getElementById('entityPersonDetailsTrigger').click();
        this.personDetails(this.userdetails);
        this.setPersonBtnRights();
    }

    addNewValue(value): void {
        this.closeModalPersonDetails.emit(value);
    }

    personDetails(userDetails): void {
        this.$subscriptions.push(this._personService.getPersonData(userDetails.personId).subscribe((data: any) => {
            this.personValues = data;
        },
        error => this._commonService.showToast(HTTP_ERROR_STATUS, 'Fetching person details failed. Please try again.')));
    }

    viewPersonDetails(personTrainingId: string): void {
        const url = this._commonService.fibiApplicationUrl + `#/fibi/person/person-details?personId=${personTrainingId}`;
        window.open(url);
    }

    setPersonBtnRights() {
        const isLoggedInPerson = this.userdetails.personId == this._commonService.currentUserDetails.personId;
        this.canShowPersonDetails = (isLoggedInPerson || this._commonService.getAvailableRight('MAINTAIN_PERSON'));
    }

    @HostListener('document:keydown.escape', ['$event'])
    handleEscapeEvent(event: any): void {
        if ((event.key === 'Escape' || event.key === 'Esc')) {
            this.addNewValue(false);
        }
    }

}
