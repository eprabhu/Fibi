import { Component, OnInit } from '@angular/core';
import { SfiService } from '../sfi.service';

@Component({
  selector: 'app-additional-sfi-details',
  templateUrl: './additional-sfi-details.component.html',
  styleUrls: ['./additional-sfi-details.component.scss']
})
export class AdditionalSfiDetailsComponent implements OnInit {

  constructor(public sfiService: SfiService) { }
  sponsorsResearch = 'Y';
  isExpanded = true;

  ngOnInit() {
  }
  
}
