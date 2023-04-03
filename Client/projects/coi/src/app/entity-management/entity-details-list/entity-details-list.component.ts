import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

@Component({
  selector: 'app-entity-details-list',
  templateUrl: './entity-details-list.component.html',
  styleUrls: ['./entity-details-list.component.scss']
})
export class EntityDetailsListComponent implements OnInit {

  entityDetails = [
    { name: 'Daniel Griffith', department: 'Research Support Office', title: 'Research Assistant', involvementStartDate: '12/08/2022', involvementEndDate: '13/06/2023', status: 'A' },
    { name: 'George Johanson', department: 'President\'s Office', title: 'Assistant Research Scientist', involvementStartDate: '15/08/2022', involvementEndDate: '04/09/2023', status: 'A' },
    { name: 'Roger Summerdon', department: 'School of Art, Design and Media', title: 'Assistant Research Scientist', involvementStartDate: '17/09/2022', involvementEndDate: '25/07/2023', status: 'A' },
    { name: 'Ian George', department: 'College Of Science', title: 'Senior Research Associate', involvementStartDate: '22/10/2022', involvementEndDate: '20/12/2023', status: 'A' },
    { name: 'Danny Johnson', department: 'NIE Office of Teacher Education', title: 'Research Associate', involvementStartDate: '24/06/2022', involvementEndDate: '13/11/2023', status: 'I' },
    { name: 'Garry Dcruz', department: 'College of Business', title: 'Research Associate', involvementStartDate: '12/12/2022', involvementEndDate: '14/03/2023', status: 'I' },
    { name: 'Dennis Daniel', department: 'Office of Finance ', title: 'Senior Research Associate', involvementStartDate: '01/08/2022', involvementEndDate: '13/10/2023', status: 'I' },
  ];
  isViewEntityDetails : true;
  isviewDetails :true;
  currentSelected = 'Person';

  constructor(private _router:Router) { }


  ngOnInit() {
  }


  viewDetails(data){
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104' } })
  }
}
