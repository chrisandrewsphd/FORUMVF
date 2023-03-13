#' Extract test level information from FORUM XML file
#'
#' @param top Root node of an xml file
#' @param comments Should the patient and text comments be extracted? Default is FALSE.
#'
#' @return a character vector with about 60 components extracted from the XML
#' @export
#'
#' @examples
#'    exdatadir <- system.file('extdata', package = 'FORUMVF')
#'    parsed <- xml2::read_xml(sprintf("%s/testdata.xml", exdatadir))
#'    root <- xml2::xml_root(parsed)
#'    xml_extract(root)
xml_extract <- function(top, comments = FALSE) {
  # Create empty return vector
  retvalnames <- c(
    "TestID",
    "Name", "MRN0", "MRN", # "MRNSource",
    "DOB", "Age", "Sex", "Institution",
    "TestDuration", "TestDate", "TestTime", "TestDateTime",
    "Laterality1", "Laterality2",
    "TestPattern", "TestStrategy",
    "Fixation1", "Fixation2", "FixationTarget",
    "FixationLossesNumer", "FixationLossesDenom",
    "FixationLossesFlagPresent", "FixationLossesFlagValue",
    "CatchTrialDataFlag",
    "FalseNegFlag", "FalseNegEst", "FalseNegTrials", "FalseNegCount",
    "FalseNegExcessFlagPresent", "FalseNegExcessFlagValue",
    "FalsePosFlag", "FalsePosEst", "FalsePosTrials", "FalsePosCount",
    "FalsePosExcessFlagPresent", "FalsePosExcessFlagValue",
    "FoveaSensitivityFlag", "FoveaSensitivity",
    "FoveaPointFlag", "FoveaPointProbValue",
    "BackgroundLuminance", "BackgroundColor",
    "MaxStimulusLuminance", "StimulusColor",
    "StimulusArea", "StimulusPresentationTime",
    "PupilDiameterLeft", "DecimalVALeft", "PupilDilatedLeft",
    "SphericalLensLeft", "CylinderLensPowerLeft", "CylinderAxisLeft",
    "PupilDiameterRight", "DecimalVARight", "PupilDilatedRight",
    "SphericalLensRight", "CylinderLensPowerRight", "CylinderAxisRight",
    "GHT", "VFI",
    "GlobalDeviation", "GlobalDeviationFlagPresent", "GlobalDeviationProbValue",
    "LocalDeviation", "LocalDeviationFlagPresent", "LocalDeviationProbValue",
    "Reliability")
  if (isTRUE(comments)) retvalnames <- c(retvalnames, "CommentsPatient", "CommentsText")

  retval <- character(length(retvalnames))
  names(retval) <- retvalnames

  computeage <- function(textdate0, textdate1) {
    rough <- as.numeric(substr(textdate1, 1, 4)) - as.numeric(substr(textdate0, 1, 4))
    if (substr(textdate1, 5, 6) < substr(textdate0, 5, 6))
      return(as.character(rough-1))
    if (substr(textdate1, 5, 6) > substr(textdate0, 5, 6))
      return(as.character(rough))
    if (substr(textdate1, 7, 8) < substr(textdate0, 7, 8))
      return(as.character(rough-1))
    return(as.character(rough))
  }
  text_of_most_common_with_check <- function(node, tag) {
    els <- xml2::xml_find_all(node, sprintf(".//attr [@tag = '%s']", tag))
    if (length(els) == 0) return(NA_character_)
    else if (length(els) == 1) return(xml2::xml_text(els))
    else {
      vec <- xml2::xml_text(els)
      tab <- sort(table(vec, exclude = c(NA, "")), decreasing = TRUE)
      if (length(tab) < 1) return("")
      else {
        if (length(tab) > 1) warning(sprintf("Multiple values are not unique for tag %s.", tag))
        return(names(tab)[1])
      }
    }
  }

  ###########
  # TEST ID #
  ###########
  # ( 0) Name: 00020003
  retval["TestID"] <- text_of_first(top, '00020003')

  ################
  # DEMOGRAPHICS #
  ################
  # ( 1) Name: 00100010
  # Name may appear several times (same tag) in different formats.
  # Up to 5 times in a sample of 10K files
  # Will use first only.
  # els <- xml_find_all(top, ".//attr [@tag = '00100010']")
  # retval["Names"] <- sapply(els, xml_text)
  retval["Name"] <- text_of_first(top, '00100010')

  # ( 2) DOB: 00100030
  # DOB may appear several times (same tag) in different formats.
  # Up to 4 times in a sample of 10K files
  # Will use first only.
  # els <- xml_find_all(top, ".//attr [@tag = '00100030']")
  # retval["DOB"] <- sapply(els, xml_text)
  retval["DOB"] <- text_of_first(top, '00100030')

  # ( 3) Sex: 00100040
  # Sex may appear several times (same tag) in different formats.
  # Up to 4 times in a sample of 10K files
  # Will use first only.
  # els <- xml_find_all(top, ".//attr [@tag = '00100040']")
  # retval["Sex"] <- sapply(els, xml_text)
  retval["Sex"] <- text_of_first(top, '00100040')

  # ( 4) MRN: 00100020
  # MRN may appear several times (same tag) in different formats.
  # Up to 4 times in a sample of 10K files
  # Get all. Give warning if not all the same.
  # Return most common
  # els <- xml_find_all(top, ".//attr [@tag = '00100020']")
  # sapply(els, xml_text)
  retval["MRN0"] <- text_of_most_common_with_check(top, '00100020')
  retval["MRN"] <- gsub("[^[:digit:]]", "", retval["MRN0"])
  # # # ( 4)* ID issuer: 00100021
  # # els <- xml_find_all(top, ".//attr [@tag = '00100021']")
  # # sapply(els, xml_text)
  # retval["MRNSource"] <- text_of_most_common_with_check(top, '00100021')

  # (XX) Institution: 00080080
  retval["Institution"] <- text_of_first(top, '00080080')

  # ( 5) Laterality: 00200060 and 00240113:Measurement Laterality
  # Laterality (for each tag) appeared at most once in sample of 10K files
  # Record first of each
  # els <- xml_find_all(top, ".//attr [@tag = '00200060']")
  # sapply(els, xml_text)
  # ( 5)* Laterality:
  # els <- xml_find_all(top, ".//attr [@tag = '00240113']")
  # sapply(els, xml_text)
  retval["Laterality1"] <- text_of_first(top, '00200060')
  retval["Laterality2"] <- text_of_first(top, '00240113')

  ###############
  # TEST TIMING #
  ###############
  # (13) Test Duration: 00240088:Visual Field Test Duration
  # (21) Date
  # (22) Time
  # Each unique in sample of 10K
  retval["TestDuration"] <- text_of_first(top, '00240088')
  retval["TestDate"] <- text_of_first(top, '00080020') # :Study Date YYYMMDD
  retval["TestTime"] <- text_of_first(top, '00080030') # :Study Time HHMMSS
  retval["TestDateTime"] <- sprintf("%s-%s-%s %s:%s:%s",
                                    substr(retval["TestDate"], 1, 4),
                                    substr(retval["TestDate"], 5, 6),
                                    substr(retval["TestDate"], 7, 8),
                                    substr(retval["TestTime"], 1, 2),
                                    substr(retval["TestTime"], 3, 4),
                                    substr(retval["TestTime"], 5, 6))
  # (23) Age
  # computed from study date and dob
  retval["Age"] <- computeage(retval["DOB"], retval["TestDate"])

  ################################
  # ( 6) "Single Field Analysis" #
  ################################

  # ( 7) Performed Protocol Code Sequence: 00400260
  # (17)
  # Each appeared exactly once in sample of 10K files
  # Use first (presumably only)
  # els <- xml_find_all(top, ".//attr [@tag = '00400260']")
  # els # nodeset
  # elsa <- xml_find_all(els, ".//attr [@tag = '00080104']") # nodeset
  # elsa
  # sapply(elsa, xml_text) # 4 elements (7) is [1], (17) is [3]
  # xml_text(xml_find_first(xml_child(els, 1), ".//attr [@tag = '00080104']")) # (7)
  # xml_text(xml_find_first(xml_child(els, 2), ".//attr [@tag = '00080104']")) # (17)
  el <- xml2::xml_find_first(top, ".//attr [@tag = '00400260']") # node with 2 children (test pattern, test strategy)
  # ch1 <- xml_child(el, 1) # node with ~4 children, ~3rd is test pattern
  # ch2 <- xml_child(el, 2) # node with ~4 children, ~3rd is test strategy
  # ch13 <- xml_find_first(ch1, ".//attr [@tag = '00080104']")
  # ch23 <- xml_find_first(ch2, ".//attr [@tag = '00080104']")
  # retval["TestPattern"] <- xml_text(ch13)
  # retval["TestStrategy"] <- xml_text(ch23)
  retval["TestPattern"] <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(el, 1), ".//attr [@tag = '00080104']"))
  retval["TestStrategy"] <- xml2::xml_text(xml2::xml_find_first(xml2::xml_child(el, 2), ".//attr [@tag = '00080104']"))

  ############
  # FIXATION #
  ############
  # ( 8) Fixation Monitoring
  # Appeared exactly once in sample of 10K files
  # Use first (presumably only)
  el <- xml2::xml_find_first(top, ".//attr [@tag = '00240032']") # node, 1 child
  ch <- xml2::xml_child(el) # node, 5 children
  gch <- xml2::xml_find_first(ch, ".//attr [@tag = '00240033']") # Code Sequence, node, 2 children
  ggch <- xml2::xml_find_all(gch, ".//attr [@tag = '00080104']")
  fmtext <- xml2::xml_text(ggch)
  if (length(fmtext) >= 1L) {
    retval["Fixation1"] <- fmtext[1]
    if (length(fmtext) >= 2L) {
      retval["Fixation2"] <- fmtext[2]
      if (length(fmtext) >= 3L) {
        warning("More than 2 Code Meanings (00080104) under Code Sequence (00240033)")
      }
    }
  }

  # (10) Fixation Losses:
  retval["FixationLossesDenom"] <- text_of_first(ch, '00240035') # denominator
  retval["FixationLossesNumer"] <- text_of_first(ch, '00240036') # numerator
  retval["FixationLossesFlagPresent"] <- text_of_first(ch, '00240039') # flag present?
  retval["FixationLossesFlagValue"] <- text_of_first(ch, '00240040') # flag value

  # ( 9) Fixation Target Type: 03051001
  retval["FixationTarget"] <- text_of_first(top, '03051001')

  #################################
  # FALSE POSITIVES AND NEGATIVES #
  #################################
  # (11) False POS
  # (12) False NEG
  # Visual Field Catch Trial Sequence
  el <- xml2::xml_find_first(top, ".//attr [@tag = '00240034']") # node, 1 child
  ch <- xml2::xml_child(el, 1) # node, ~13 children

  retval["CatchTrialDataFlag"] <- text_of_first(el, '00240055') # Catch Trials Data Flag (100% YES in sample data)
  retval["FalseNegFlag"] <- text_of_first(ch, '00240045') #a False Negatives Estimate Flag
  retval["FalseNegEst"] <- text_of_first(ch, '00240046') #b False Negatives Estimate
  retval["FalseNegTrials"] <- text_of_first(ch, '00240048') #c Negative Catch Trials Quantity
  retval["FalseNegCount"] <- text_of_first(ch, '00240050') #d False Negatives Quantity
  retval["FalseNegExcessFlagPresent"] <- text_of_first(ch, '00240051') #e Excessive False Negatives Data Flag
  retval["FalseNegExcessFlagValue"] <- text_of_first(ch, '00240052') #f Excessive False Negatives
  retval["FalsePosFlag"] <- text_of_first(ch, '00240053') #a False Positives Estimate Flag
  retval["FalsePosEst"] <- text_of_first(ch, '00240054') #b False Positives Estimate
  retval["FalsePosTrials"] <- text_of_first(ch, '00240056') #c Positive Catch Trials Quantity
  retval["FalsePosCount"] <- text_of_first(ch, '00240060') #d False Positives Quantity
  retval["FalsePosExcessFlagPresent"] <- text_of_first(ch, '00240061') #e Excessive False Positives Data Flag
  retval["FalsePosExcessFlagValue"] <- text_of_first(ch, '00240062') #f Excessive False Positives



  #########
  # FOVEA #
  #########
  # (14) Fovea
  retval["FoveaSensitivityFlag"] <- text_of_first(top, '00240086') # Foveal Sensitivity Measured
  retval["FoveaSensitivity"] <- text_of_first(top, '00240087') # Foveal Sensitivity
  retval["FoveaPointFlag"] <- text_of_first(top, '00240117') # Foveal Sensitivity Measured
  retval["FoveaPointProbValue"] <- text_of_first(top, '00240118') # Foveal Sensitivity

  ###########################
  # STIMULUS AND BACKGROUND #
  ###########################
  # (16) Background
  retval["BackgroundLuminance"] <- text_of_first(top, '00240020') # Background Luminance
  el <- xml2::xml_find_first(top, ".//attr [@tag = '00240024']") # Background Illumination Color Code Sequence
  retval["BackgroundColor"] <- text_of_first(el, '00080104') # color

  # (15) Stimulus
  retval["MaxStimulusLuminance"] <- text_of_first(top, '00240018') # Maximum Stimulus Luminance
  el <- xml2::xml_find_first(top, ".//attr [@tag = '00240021']") # Stimulus Color Code Sequence
  retval["StimulusColor"] <- text_of_first(el, '00080104') # color
  retval["StimulusArea"] <- text_of_first(top, '00240025') # Stimulus Area
  retval["StimulusPresentationTime"] <- text_of_first(top, '00240028') # Stimulus Presentation Time

  # POINTWISE RESULTS
  # 00240089: node with many children
  # # MANY (e.g., 54) OF THE FOLLOWING:
  # <!--Visual Field Test Point X-Coordinate--> <attr len="4" vr="FL" tag="00240090">9.0</attr>
  # <!--Visual Field Test Point Y-Coordinate-->	<attr len="4" vr="FL" tag="00240091">9.0</attr>
  # <!--Stimulus Results--> <attr len="4" vr="CS" tag="00240093">SEEN</attr>
  # <!--Sensitivity Value--> <attr len="4" vr="FL" tag="00240094">31.0</attr>
  # <!--Visual Field Test Point Normals Sequence--> -<attr len="-1" vr="SQ" tag="00240097">
  # -<item>
  # 	<!--Age Corrected Sensitivity Deviation Value--> <attr len="4" vr="FL" tag="00240092">0.0</attr>
  # 	<!--Age Corrected Sensitivity Deviation Probability Value-->	<attr len="4" vr="FL" tag="00240100">0.0</attr>
  # 	<!--Generalized Defect Corrected Sensitivity Deviation Flag-->	<attr len="4" vr="CS" tag="00240102">YES</attr>
  # 	<!--Generalized Defect Corrected Sensitivity Deviation Value-->	<attr len="4" vr="FL" tag="00240103">1.0</attr>
  # 	<!--Generalized Defect Corrected Sensitivity Deviation Probability Value-->	<attr len="4" vr="FL" tag="00240104">0.0</attr>
  # </item>
  # els <- xml_find_all(top, ".//attr [@tag = '00240093']") # Stimulus Results
  # sapply(els, xml_text)
  # els <- xml_find_all(top, ".//attr [@tag = '00240095']") # Retest Stimulus Results
  # sapply(els, xml_text)


  # (17) Strategy: See (7) above

  #######
  # EYE #
  #######
  # (20) Rx?
  # (18) Pupil Diameter, (19) Decimal Visual Acuity, and others
  # Most files have either L or R. But a few have both.
  # (never two L or two R)
  # Store L and R separately

  # Ophthalmic Patient Clinical Information Left Eye Sequence
  left <- xml2::xml_find_first(top, ".//attr [@tag = '00240114']") # node, 1 child

  if (!is.na(left)) {
    retval["PupilDiameterLeft"] <- text_of_first(left, '00460044') # Pupil Size
    retval["DecimalVALeft"] <- text_of_first(left, '00460137') # Decimal VA
    retval["SphericalLensLeft"] <- text_of_first(left, '00220007') # Spherical Lens Power
    retval["CylinderLensPowerLeft"] <- text_of_first(left, '00220008') # Cylinder Lens Power
    retval["CylinderAxisLeft"] <- text_of_first(left, '00220009') # Cylinder Axis
    retval["PupilDilatedLeft"] <- if (is.na(text_of_first(left, '0022000D'))) "NO" else "YES" # Pupil Dilated
  }

  # Ophthalmic Patient Clinical Information Right Eye Sequence
  right <- xml2::xml_find_first(top, ".//attr [@tag = '00240115']") # node, 1 child

  if (!is.na(right)) {
    retval["PupilDiameterRight"] <- text_of_first(right, '00460044') # Pupil Size
    retval["DecimalVARight"] <- text_of_first(right, '00460137') # Decimal VA
    retval["SphericalLensRight"] <- text_of_first(right, '00220007') # Spherical Lens Power
    retval["CylinderLensPowerRight"] <- text_of_first(right, '00220008') # Cylinder Lens Power
    retval["CylinderAxisRight"] <- text_of_first(right, '00220009') # Cylinder Axis
    retval["PupilDilatedRight"] <- if (is.na(text_of_first(right, '0022000D'))) "NO" else "YES" # Pupil Dilated
  }

  # (24) GHT:
  el <- xml2::xml_find_first(top, ".//attr [@tag = '0040A168']") # Concept Code Sequence
  retval["GHT"] <- text_of_first(el, '00080104')


  # (25) VFI:
  retval["VFI"] <- text_of_first(top, '0040A30A') # "Numeric Value" within "Visual Field Global Results Index Sequence"
  # otherwise could follow the path to it
  # els <- xml_find_all(top, ".//attr [@tag = '00240320']") # :
  # xml_children(els[[1]]) # 2 children
  # xml_child(els[[1]], 1)
  # xml_find_all(els[[1]], ".//attr [@tag = '00240325']")
  # xml_find_first(xml_child(els[[1]], 1), ".//attr [@tag = '00240325']")
  # xml_find_first(xml_child(els[[1]], 2), ".//attr [@tag = '00240325']")
  # xml_text(xml_child(xml_child(xml_child(xml_child(els[[1]], 1), 1), 1), 4)) # use path?

  # (26) MD:
  retval["GlobalDeviation"] <- text_of_first(top, '00240066') # Global Deviation From Normal
  retval["GlobalDeviationFlagPresent"] <- text_of_first(top, '00240059') # Global Deviation Probability Probability Normals Flag
  retval["GlobalDeviationProbValue"] <- text_of_first(top, '00240071') # Global Deviation Probability

  # (27) PSD:
  retval["LocalDeviation"] <- text_of_first(top, '00240068') # Localized Deviation From Normal
  retval["LocalDeviationFlagPresent"] <- text_of_first(top, '00240072') # Local Deviation Probability Probability Normals Flag
  retval["LocalDeviationProbValue"] <- text_of_first(top, '00240073') # Local Deviation Probability

  # (28) Reliability
  retval["Reliability"] <- text_of_first(top, '00240069') # Patient Reliability Indicator

  # (29) Comments
  if (isTRUE(comments)) {
    retval["CommentsPatient"] <- text_of_first(top, '00104000') # Patient Comments
    retval["CommentsText"] <- text_of_first(top, '40004000') # Text Comments
  }

  return(retval)
}
