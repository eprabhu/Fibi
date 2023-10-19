/* tslint:disable:no-unused-variable */

import { TestBed, async, inject } from '@angular/core/testing';
import { TravelRiskSliderService } from './travel-risk-slider.service';

describe('Service: TravelRiskSlider', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [TravelRiskSliderService]
    });
  });

  it('should ...', inject([TravelRiskSliderService], (service: TravelRiskSliderService) => {
    expect(service).toBeTruthy();
  }));
});
